(* server_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open String 
open Game
open Unix


let get_today uri = match uri with 
  | x  -> String.sub x 17 3
  | _ -> ""

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let _ = print_endline uri in 
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      let resp = Lwt_main.run (get_today uri |> get_todays_game ) in 
      resp)
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)