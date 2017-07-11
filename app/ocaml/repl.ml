open Game
open Cohttp
open Cohttp_lwt_unix
open Lwt
open Yojson.Basic.Util
open Unix

type verb = Help| Scores | Stop

type command = {
  verb : verb;
  obj : string
}

(* [Illegal] is raised by [do'] to indicate that a command is illegal;
 * see the documentation of [do'] below. *)
exception Illegal

(* [Quit] is raised when the  "quit" command is entered *)
exception Quit

(**** Helper Functions ****)

(* this line is so that I can use Yojson without
 * writing Yojson.Basic.Util so many times *)
open Yojson.Basic.Util

(* [to_verb str] is the verb type represented by the [str] *)
let to_verb = function
  | "help" -> Help
  | "score" -> Scores
  | "quit" -> Stop
  | _ -> raise (Illegal)

(* [to_command com] is the command object stored in string [com] *)
let to_command com =
  let str_trimmed = String.trim com in
  let str_com = String.lowercase_ascii str_trimmed in
    if String.contains str_com ' ' then
      let int_index = String.index str_com ' ' in
      let str_fst = String.sub str_com 0 int_index in
      let int_snd_length = (String.length str_com) - int_index in
      let str_snd = String.sub str_com (int_index + 1) (int_snd_length - 1) in
        { verb = to_verb str_fst; obj = str_snd }
    else
      { verb = to_verb str_com; obj = "" }

(* [help_action com] is the unit type resulting from displaying 
 * help information to the user *)
let help_action com = 
  print_endline "type help for help, score <team-name> for team score and quit to quit"

(* [scores_action com] is the unit type resulting from running 
 * the [get_todays_game] function in game.ml on command [com]
 * to get and print today's baseball scores *)
let scores_action com = 
  let today_score = Lwt_main.run (get_todays_game com.obj) in
    print_endline (today_score)

(* [quit_action com] raises a quit exception, ending the game *)
let quit_action com = 
  raise (Quit)

(* [process c] is the unit type resulting from processing a command *)
let process c = 
  let com = to_command c in 
  match com.verb with
  | Help -> help_action com
  | Scores -> scores_action com
  | Stop -> quit_action com

(* [repl st] is a unit value that acts as the looping mechanism
 * in the Read Eval Print Loop of the terminal. Uses user input to 
 * execute a command and then calls itself again on that state. 
 * Exception handling is also implemented, in the case that a user quits,
 * or another exception is thrown. *)
let rec repl s =
  try
  print_string  "> ";
  let input = read_line () in
  let _ = process input in
    repl s
  with
  | Illegal -> let _ = print_endline "An error occured" in
    repl s
  | Quit -> let _ = print_endline "Good bye" in ()

let () =
  ANSITerminal.(print_string [red] 
    "\n\nWelcome to MLB terminal.\n");
  print_endline "What's your name?";
  print_string  "> ";
  let name = read_line () in
  	let _ = print_endline ("Hello " ^ name) in 
    print_endline "Please enter a command";
    repl name
