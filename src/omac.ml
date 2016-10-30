open Lwt
open Cohttp
open Cohttp_lwt_unix

let getenv key default = try Sys.getenv key with Not_found -> default
let getenv_opt key = try Some (Sys.getenv key) with Not_found -> None

let port            = getenv "PORT" "8999" |> int_of_string
let shared_key      = getenv "OMAC_KEY" "IDEAD9ACCDB7AD845243CE2EF295"
let max_redirects   = getenv "OMAC_MAX_REDIRECTS" "4" |> int_of_string
let socket_timeout  = getenv "OMAC_SOCKET_TIMEOUT" "10" |> int_of_string
let keep_alive      = getenv "OMAC_KEEP_ALIVE", "false" |> bool_of_string
let accept          = getenv_opt "OMAC_ACCEPT"
let accept_encoding = getenv_opt "OMAC_ACCEPT_ENCODING"
let user_agent      = getenv "OMAC_HEADER_VIA" "omac proxy"
let content_length_limit = getenv "OMAC_CONTENT_LENGTH_LIIT" "5242880" |> int_of_string


let int_of_string_ign s =
  try
    Some (int_of_string s)
  with Failure _ -> None

let bad_request msg =
  Server.respond_error ~headers:(Header.init ()) ~status:`Bad_request ~body:(Printf.sprintf "Bad request: %s" msg) ()


let default_header =
  Header.of_list [
    ("X-Frame-Options", "deny");
    ("X-XSS-Protection", "1; mode=block");
    ("X-Content-Type-Options", "nosniff");
    ("Content-Security-Policy", "default-src 'none'; img-src data:; style-src 'unsafe-inline'");
    ("Strict-Transport-Security", "max-age=31536000; includeSubDomains");
  ]


let respond_empty msg uri =
  Lwt_log.ign_error_f "%s (%s)" msg (Uri.to_string uri);
  Server.respond ~status:`Not_found ~headers:default_header ~body:Cohttp_lwt_body.empty ()


let manage_content_length src_headers =
  match Header.get src_headers "content-length" with
  | Some value ->
      begin match int_of_string_ign value with
      | Some i ->
          if i > content_length_limit then
            Error "content-length-limit exceeded"
          else
            Ok ("content-length", value)
      | None -> 
            Error "content-limit is not int"
      end
  | None ->
      match Header.get src_headers "transfer-encoding" with
      | Some value -> Ok ("transfer-encoding", value)
      | None -> 
        match Header.get src_headers "content-encoding" with
        | Some value -> Ok ("content-encoding", value)
        | None -> Error "missing content-length"


let build_response_header src_headers =
  match manage_content_length src_headers with
  | Error _ as e ->  e
  | Ok content_length ->
    match Header.get src_headers "content-type" with
    (* TODO LOOKUP MIME-TYPE *)
    | None -> Error "response header not contain content-type"
    | Some content_type ->
      let cache_control = (match Header.get src_headers "cache-control" with | Some s -> s | None -> "public, max-age=31536000") in
      let extra = List.fold_left (fun header key ->
        match Header.get src_headers key with
        | Some value -> (key, value)::header
        | None -> header)
      [("content-type", content_type); ("cache-control", cache_control); content_length]
      ["etag"; "expires"; "last-modified"]
      in
      let new_header = Header.add_list default_header extra in
      Lwt_log.ign_debug_f "response_header\n%s" (Header.to_string new_header);
      Ok new_header


let is_invalid_path path =
   path = "/" || path = "/favicon.ico"


let hmac url =
  let open Cryptokit in
  let hash = MAC.hmac_sha1 shared_key in
  hash#add_string url;
  let t = Hexa.encode () in
  t#put_string hash#result;
  t#get_string


let parse_uri uri =
  let path = Uri.path uri in
  match Str.split(Str.regexp_string "/") path with
  | [] -> None
  | digest::encoded_url::[] -> Some (digest, encoded_url)
  | _ -> 
      begin match Uri.get_query_param uri "url" with
      | Some encoded_url -> (* remove / *)
          Some (String.sub path 1 ((String.length path) - 1), encoded_url)
      | None -> None
      end


let compare_hmac digest src_url =
  let hash = hmac src_url in
  Lwt_log.ign_debug_f "compare hash %s = %s" hash digest;
  hash = digest


let request_from_self req =
  match Header.get (Request.headers req) "via" with
  | None -> false
  | Some via ->via = user_agent

let redirect_to resp orig_url =
  match Header.get (Response.headers resp) "location" with
  | None -> Error "Redirect with no location"
  | Some location ->
      let new_url = Uri.of_string location in
      match Uri.host new_url with
      | None ->
          Ok (Uri.with_scheme
          (Uri.with_host new_url (Uri.host orig_url))
          (Uri.scheme orig_url))
      | Some _ -> Ok new_url


let rec process_url src_url transferred_headers remaining_redirects =
  Lwt_log.ign_info_f "forward to %s" (Uri.to_string src_url);
  Lwt_log.ign_debug_f "transferred_headers\n%s" (Header.to_string transferred_headers);
  Client.get ~headers:transferred_headers src_url >>= (fun (resp, body) ->
    match Response.status resp with
    | `Moved_permanently | `Found | `See_other | `Temporary_redirect ->
        if remaining_redirects <= 0 then
          return_error "Exceeded max depth"
        else
          begin match redirect_to resp src_url with
          | Error e -> return_error e
          | Ok new_url ->
              Lwt_log.ign_debug_f "redirect_to %s" (Uri.to_string new_url);
              process_url new_url transferred_headers (remaining_redirects - 1)
          end
    | status ->
      match build_response_header (Response.headers resp) with
      | Error msg -> return_error msg
      | Ok header ->
          if status == `Not_modified then
            return_ok (Server.respond ~status ~headers:header ~body ())
          else
            return_ok (Server.respond ~status ~headers:header ~body ())
  )


let transferred_headers req =
  let headers = Request.headers req in
  let accept_val = match accept with
  | Some s -> s
  | None -> match Header.get headers "accept" with | Some s -> s | None -> "image/*"
  in
  let accept_encoding_val = match accept_encoding with
  | Some s -> s
  | None -> match Header.get headers "accept-encoding" with | Some s -> s | None -> ""
  in
  let default_header = [
    "Via", user_agent;
    "User-Agent", user_agent;
    "Accept", accept_val;
    "Accept-Encoding",  accept_encoding_val;
    "X-Frame-Options", "deny";
    "X-XSS-Protection", "1; mode=block";
    "X-Content-Type-Options", "nosniff";
    "Content-Security-Policy", "default-src 'none'; img-src data:; style-src 'unsafe-inline'";
    "Strict-Transport-Security", "max-age=31536000; includeSubDomains";
  ]
  in
  let new_header = List.fold_left (fun header key ->
    match Header.get headers key with
    | Some v -> (key, v)::header
    | None -> header)
  default_header
  ["etag"; "expires"; "last-modified"]
  in
  Header.of_list new_header

let transferred_headers req =
  let headers = Request.headers req in
  let accept_val = match accept with
  | Some s -> s
  | None -> match Header.get headers "accept" with | Some s -> s | None -> "image/*"
  in
  let accept_encoding_val = match accept_encoding with
  | Some s -> s
  | None -> match Header.get headers "accept-encoding" with | Some s -> s | None -> ""
  in
  Header.of_list ["Via", user_agent;
    "User-Agent", user_agent;
    "Accept", accept_val;
    "Accept-Encoding",  accept_encoding_val;
    "X-Frame-Options", "deny";
    "X-XSS-Protection", "1; mode=block";
    "X-Content-Type-Options", "nosniff";
    "Content-Security-Policy", "default-src 'none'; img-src data:; style-src 'unsafe-inline'";
  ]

let server () =
  let callback _conn req _body =
    let uri = Request.uri req in
    match req |> Request.meth with
    | `GET ->
        Lwt_log.ign_info_f "GET %s" (Uri.to_string uri);
        if is_invalid_path (Uri.path uri) || request_from_self req then
          respond_empty "invalid path" uri
        else
          begin match parse_uri uri with
          | None ->
              respond_empty "invalid url" uri
          | Some (digest, src_url) ->
              if compare_hmac digest src_url then
                process_url (Uri.of_string src_url) (transferred_headers req) max_redirects >>= (function
                  | Ok resp_body -> resp_body
                  | Error msg -> respond_empty msg uri
                )
              else
                respond_empty "checksum unmatch" uri
          end
    | _  -> respond_empty "unsupported method" uri
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())


let run () =
  ignore (Lwt_main.run (server ()))
