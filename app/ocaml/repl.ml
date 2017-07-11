type verb = Go | Take | Drop | Look | Inventory | Score | Turns | Stop

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
  | "go" -> Go
  | "take" -> Take
  | "drop" -> Drop
  | "look" -> Look
  | "inv" | "inventory" -> Inventory
  | "score" -> Score
  | "turns" -> Turns
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
    else if str_com = "north" then
      { verb = to_verb "go"; obj = "north" }
    else if str_com = "south" then
      { verb = to_verb "go"; obj = "south" }
    else if str_com = "east" then
      { verb = to_verb "go"; obj = "east" }
    else if str_com = "west" then
      { verb = to_verb "go"; obj = "west" }
    else if str_com = "up" then
      { verb = to_verb "go"; obj = "up" }
    else if str_com = "down" then
      { verb = to_verb "go"; obj = "down" }
    else
      { verb = to_verb str_com; obj = "" }

(* [quit_action] raises a quit exception, ending the game *)
let quit_action c = 
  raise (Quit)

let go_action g = print_endline g

(* [do' c st] is [st'] if it is possible to do command [c] in
 * state [st] and the resulting new state would be [st'].  The
 * function name [do'] is used because [do] is a reserved keyword.
 *   - The "go" (and its shortcuts), "take" and "drop" commands
 *     either result in a new state, or are not possible because
 *     their object is not valid in state [st] hence they raise [Illegal].
 *       + the object of "go" is valid if it is a direction by which
 *         the current room may be exited
 *       + the object of "take" is valid if it is an item in the
 *         current room
 *       + the object of "drop" is valid if it is an item in the
 *         current inventory
 *       + if no object is provided (i.e., the command is simply
 *         the bare word "go", "take", or "drop") the behavior
 *         is unspecified
 *   - The "quit", "look", "inventory", "inv", "score", and "turns"
 *     commands are always possible and leave the state unchanged.
 *   - The behavior of [do'] is unspecified if the command is
 *     not one of the commands given in the assignment writeup.
 * The underspecification above is in order to enable karma
 * implementations that provide new commands. *)
let process c =
  let com = to_command c in
  match com.verb with
  | Go -> go_action c
  | Stop -> quit_action c
  | _ -> print_endline c

(* [repl st] is a unit value that acts as the looping mechanism
 * in the Read Eval Print Loop of the terminal. Uses user input to 
 * execute a command and create a new state,
 * then calls itself again on that state. Exception handling
 * is also implemented, in the case that a user quits the game,
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
  print_endline "What's your name?\n";
  print_string  "> ";
  let name = read_line () in
  	let _ = print_endline ("Hello " ^ name) in 
    print_endline "Please enter a command";
    repl name
