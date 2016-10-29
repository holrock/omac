open Lwt
open Cohttp
open Cohttp_lwt_unix

let image_url = "https://localhost/test.jpg"

let getenv key default = try Sys.getenv key with Not_found -> default
let getenv_opt key = try Some (Sys.getenv key) with Not_found -> None

let port            = getenv "PORT" "8999" |> int_of_string
let shared_key      = getenv "OMAC_KEY" "IDEAD9ACCDB7AD845243CE2EF295"
let max_redirects   = getenv "OMAC_MAX_REDIRECTS" "4" |> int_of_string
let socket_timeout  = getenv "OMAC_SOCKET_TIMEOUT" "10" |> int_of_string
let keep_alive      = getenv "OMAC_KEEP_ALIVE", "false" |> bool_of_string
let accept          = getenv_opt "OMAC_ACCEPT"
let accept_encoding = getenv_opt "OMAC_ACCEPT_ENCODING"


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

let respond_empty () =
  Server.respond ~status:`OK ~headers:default_header ~body:Cohttp_lwt_body.empty ()

let build_response_header src_resp =
  match Header.get src_resp "content-type" with
  Some content_type ->
    let cache_control = (match Header.get src_resp "cache-control" with | Some s -> s | None -> "public, max-age=31536000") in
    let new_header = Header.add_list default_header [("content-type", content_type); ("cache-control", cache_control)] in
    Some new_header
  | None -> None

let is_invalid_path path =
   path = "/" || path = "/favicon.ico"

let hmac url =
  let open Cryptokit in
  let hash = MAC.hmac_sha1 shared_key in
  hash#add_string url;
  let t = Hexa.encode () in
  t#put_string hash#result;
  t#get_string

let server () =
  let callback _conn req _body =
    match req |> Request.meth with
    | `GET ->
        let path = req |> Request.uri |> Uri.path in
        if is_invalid_path path then
          respond_empty ()
        else
          Client.get (Uri.of_string image_url) >>= (fun (resp, body) ->
            begin match build_response_header (Response.headers resp) with
            | Some header -> Server.respond ~status:`OK ~headers:header ~body ()
            | None -> respond_empty ()
          end)
    | _  -> respond_empty ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let run () =
  ignore (Lwt_main.run (server ()))
