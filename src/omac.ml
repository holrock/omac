open Lwt
open Cohttp
open Cohttp_lwt_unix

let image_url = "https://localhost/test.jpg"

let bad_request msg =
  Server.respond_error ~headers:(Header.init ()) ~status:`Bad_request ~body:(Printf.sprintf "Bad request: %s" msg) ()

let fetch_resource uri _req =
  Client.get (Uri.of_string uri)

let default_header =
  Header.of_list [
    ("X-Frame-Options", "deny");
    ("X-XSS-Protection", "1; mode=block");
    ("X-Content-Type-Options", "nosniff");
    ("Content-Security-Policy", "default-src 'none'; img-src data:; style-src 'unsafe-inline'");
    ("Strict-Transport-Security", "max-age=31536000; includeSubDomains");
  ]

let respond_empty () = Server.respond ~status:`OK ~headers:default_header ~body:Cohttp_lwt_body.empty ()

let build_response_header src_resp =
  match Header.get src_resp "content-type" with
  Some content_type ->
    let cache_control = (match Header.get src_resp "cache-control" with | Some s -> s | None -> "public, max-age=31536000") in
    let new_header = Header.add_list default_header [("content-type", content_type); ("cache-control", cache_control)] in
    Some new_header
  | None -> None

;;

let server () =
  let callback _conn req _body =
    let _path = req |> Request.uri |> Uri.path in
    match req |> Request.meth with
    | `GET ->
        let proxy_body = fetch_resource image_url req in
        proxy_body >>= (fun (resp, body) ->
          begin match build_response_header (Response.headers resp) with
          | Some header -> Server.respond ~status:`OK ~headers:header ~body ()
          | None -> respond_empty ()
          end)
    | _  -> respond_empty ()
  in
  Server.create ~mode:(`TCP (`Port 8999)) (Server.make ~callback ())

let () = ignore (Lwt_main.run (server ()))
