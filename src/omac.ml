open Lwt
open Cohttp
open Cohttp_lwt_unix

let bad_request msg =
  Server.respond_error ~headers:(Header.init ()) ~status:`Bad_request ~body:(Printf.sprintf "Bad request: %s" msg) ()

let server () =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    (*match req |> Request.meth with
    | `GET ->
    | _ -> *)bad_request "invalid method"
    
  in
  Server.create ~mode:(`TCP (`Port 8999)) (Server.make ~callback ())

let () = ignore (Lwt_main.run (server ()))
