type exit = {
  exit_direction       : string;
  exit_room_id         : string
}

type item = {
  item_id              : string;
  item_description     : string;
  item_points          : int
}

type room = {
  room_id              : string;
  room_description     : string;
  room_points          : int;
  room_exits           : exit list;
  room_treasure        : string list
}

type location = {
  item            : item;
  room            : room
}

type map = {
  map_rooms       : room list;
  map_items       : item list;
}

(* You may redefine [state] to be whatever type
 * you wish, but do not change its name. *)
type state = {
  max_score       : int;
  score           : int;
  turns           : int;
  current_room_id : string;
  inv             : item list;
  visited         : room list;
  locations       : (item * room) list;
  map             : map
}

type verb = Go | Take | Drop | Look | Inventory | Score | Turns | Quit

type command = {
  verb : verb;
  obj : string
}

(* [Illegal] is raised by [do'] to indicate that a command is illegal;
 * see the documentation of [do'] below. *)
exception Illegal

(* [Quit] is raised when the  "quit" command is entered *)
exception Quit

(* [InvalidDirection] is raised when an invalid direction is entered
 * with a "go" command *)
exception InvalidDirection

(* [InvalidItem] is raised when a user tries to take or drop an item that
 * does not exist in their current room, or their inventory respectively *)
exception InvalidItem

(**** Helper Functions ****)

(* this line is so that I can use Yojson without
 * writing Yojson.Basic.Util so many times *)
open Yojson.Basic.Util


(**** Functions to make types (aside from state) ****)

(* [to_exit exit] is the exit type represented by the JSON
 * element [exit] *)
let to_exit exit =
  {
    exit_direction = exit |> member "direction" |> to_string;
    exit_room_id = exit |> member "room_id" |> to_string
  }

(* [to_item item] is an item type represented by the JSON
 * array element [item] *)
let to_item item =
  {
    item_id = item |> member "id" |> to_string;
    item_description = item |> member "description" |> to_string;
    item_points = item |> member "points" |> to_int
  }

(* [to_room room] is a room type represented by the JSON
 * array element [room] *)
let to_room room =
  {
    room_id = room |> member "id" |> to_string;
    room_description = room |> member "description" |> to_string;
    room_points  = room |> member "points" |> to_int;
    room_exits = List.map to_exit (room |> member "exits" |> to_list);
    room_treasure = List.map to_string (room |> member "treasure" |> to_list)
  }

(* [to_map j] is a map type represented by the JSON
 * object [j] *)
let to_map j =
  {
    map_rooms = List.map to_room (j |> member "rooms" |> to_list);
    map_items = List.map to_item (j |> member "items" |> to_list)
  }

(* [to_verb str] is the verb type represented by the [str] *)
let to_verb = function
  | "go" -> Go
  | "take" -> Take
  | "drop" -> Drop
  | "look" -> Look
  | "inv" | "inventory" -> Inventory
  | "score" -> Score
  | "turns" -> Turns
  | "quit" -> Quit
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

(**** End Functions to make types ****)

(* [points_list j spec] is the int list made from
 * JSON object [j], with a JSON array stored in the
 * string key [spec]
 * requires:
 *  - each JSON object in [spec] in [j] must have a "points" field *)
let points_from_list j spec =
  let pts_list = j |> member spec |> to_list in
  let pts_values = List.map (member "points") pts_list in
  let pts_ints = List.map to_int pts_values in
    List.fold_left (+) 0 pts_ints

(* [string_list lst spec] is the string list version of the
 * Yojson list [lst], with the JSON array stored in the
 * string key [spec] *)
let string_list lst spec =
  let items_lst = List.map (member spec) lst in
    List.map to_string items_lst

(* [get_item_id item] is the id field of [item]
 * requires: [item] is of type item *)
let get_item_id item = item.item_id

(* [get_room_id room] is the id field of [room]
 * requires: [room] is of type room *)
let get_room_id room = room.room_id

(* [get_exit_direction exit] is the string direction field of [exit]
 * requires: [exit] is of type exit *)
let get_exit_direction exit = exit.exit_direction

(* [get_item_from_location location] is the item field of [location]
 * requires: [location] is associative list (item * room) *)
let get_item_from_location location = fst(location)

(* [value_from_id values id] is the JSON object associated with the [id]
 * inside of the JSON list of values [values] *)
let rec value_from_id values id = match values with
  | [] -> raise (Illegal)
  | h :: t when (h |> member "id" |> to_string) = id -> h
  | h :: t -> value_from_id t id

(* [room_contains_item room location] is a boolean respresenting
 * whether or not the given JSON [room] object has the item at the
 * [location] in its treasure *)
let room_contains_item room location =
  let treasure = List.map to_string (room |> member "treasure" |> to_list) in
    List.mem (location |> member "item" |> to_string) treasure

(* [check_locations j locations] is list of item id's who are in the
 * right location specified by the JSON list [locations] and
 * the JSON game state [j] *)
let rec check_locations j locations = match locations with
  | [] -> []
  | h :: t ->
    let room_id = h |> member "room" |> to_string in
    let rooms = j |> member "rooms" |> to_list in
    let room = value_from_id rooms room_id in
      if (room_contains_item room h) then
        (h |> member "item" |> to_string) :: check_locations j t
      else check_locations j t

(* [make_item_list items item_ids] is a list of json item objects
 * constructed from the list of [items_ids] and the list of
 * all [items] *)
let rec make_item_list items item_ids = match item_ids with
  | [] -> []
  | h :: t -> (value_from_id items h) :: make_item_list items t

(* [points_of_item item] is the points associated with [item] *)
let points_of_item item = item |> member "points" |> to_int

(* [calc_total_item_points j] is the total number of points from
 * the intial locations of items specified by JSON object [j] *)
let calc_total_item_points j =
  let start_locations = j |> member "start_locations" |> to_list in
  let items_to_sum_ids = check_locations j start_locations in
  let items_to_sum = make_item_list
            (j |> member "items" |> to_list) items_to_sum_ids in
  let items_points = List.map points_of_item items_to_sum in
    List.fold_left (+) 0 items_points

(* [find_max_score j] is the max score of the game
 * represented by JSON object [j] *)
let find_max_score j =
  let room_pts = points_from_list j "rooms" in
  let item_pts = points_from_list j "items" in
    room_pts + item_pts

(* [find_current_room_id] is the id of current room of the game
 * represented by JSON object [j] *)
let find_current_room_id j = j |> member "start_room" |> to_string

(* [calc_start_score j] is the initial score of the game
 * represented by JSON object [j] *)
let calc_start_score j =
  let room_id = find_current_room_id j in
  let room_list = j |> member "rooms" |> to_list in
  let room = value_from_id room_list room_id in
  let room_points = room |> member "points" |> to_int in
  let total_item_points = calc_total_item_points j in
    room_points + total_item_points

(* [make_inv j] is the current inventory of the game
 * represented by JSON object [j] *)
let make_inv j =
  let inv_list = List.map to_string (j |> member "start_inv" |> to_list) in
  let items = j |> member "items" |> to_list in
  let list_json_inv = make_item_list items inv_list in
    List.map (to_item) list_json_inv

(* [make_visited j] is the list of rooms that have been
 * visited, specified by JSON object [j] *)
let make_visited j =
  let str_current_room = find_current_room_id j in
  let json_list_rooms = j |> member "rooms" |> to_list in
  let json_room_current = value_from_id json_list_rooms str_current_room in
  let room_current = to_room json_room_current in
    room_current :: []

(* [make_locations j] is the association list mapping item
 * id's to the id of the room in which they are currently located,
 * according to JSON object [j] *)
let make_locations j =
  let lst = j |> member "start_locations" |> to_list in
  let list_string_item_ids = string_list lst "item" in
  let list_all_json_items = j |> member "items" |> to_list in
  let list_specific_json_items =
          make_item_list list_all_json_items list_string_item_ids in
  let list_items = List.map (to_item) list_specific_json_items in
  let list_string_room_ids = string_list lst "room" in
  let list_all_json_rooms = j |> member "rooms" |> to_list in
  let list_specific_json_rooms =
          make_item_list list_all_json_rooms list_string_room_ids in
  let list_rooms = List.map (to_room) list_specific_json_rooms in
    List.combine list_items list_rooms

(* [room_from_id room_list id] is the room that is inside of
 * [room_list] with [id] *)
let rec room_from_id room_list id = match room_list with
  | [] -> raise (Illegal)
  | h :: t when h.room_id = id -> h
  | h :: t -> room_from_id t id

(* [item_from_id item_list id] is the item that is inside of
 * [item_list] with [id] *)
let rec item_from_id item_list id = match item_list with
  | [] -> raise (InvalidItem)
  | h :: t when h.item_id = id -> h
  | h :: t -> item_from_id t id

(* [get_exit_object direction exits] is the exit type associated with
 * the [direction] in the exit list [exits] *)
let rec get_exit_object direction exits = match exits with
  | [] -> raise (Illegal)
  | h :: t when h.exit_direction = direction -> h
  | h :: t -> get_exit_object direction t

(* [get_location_object item locations] is the location type associated with
 * the [item] in the location list (item*room) [locations] *)
let rec get_location_object item locations = match locations with
  | [] -> raise (Illegal)
  | h :: t when fst(h) = item -> {item = fst(h); room = snd(h)}
  | h :: t -> get_location_object item t

(* [remove_from_list val lst] is a new list idential to
 * [lst] except with the item [val] removed *)
let rec remove_from_list item lst = match lst with
  | [] -> []
  | h :: t when item = h -> remove_from_list item t
  | h :: t -> h :: remove_from_list item t

(* [print_inv_list lst] is unit and print every item
 * in the [lst] *)
let rec print_inv_list lst = match lst with
  | [] -> print_string "";
  | h :: t -> let _ = print_endline h.item_id in
    print_inv_list t

(**** End Helper Functions ****)

(* [init_state j] is the initial state of the game as
 * determined by JSON object [j] *)
let init_state j =
  let max_score = find_max_score j in
  let score = calc_start_score j in
  let turns = 0 in
  let current_room_id = find_current_room_id j in
  let inv = make_inv j in
  let visited = make_visited j in
  let locations = make_locations j in
  let map = to_map j in

    {
      max_score = max_score;
      score = score;
      turns = turns;
      current_room_id = current_room_id;
      inv = inv;
      visited = visited;
      locations = locations;
      map = map
    }

(* [max_score s] is the maximum score for the adventure whose current
 * state is represented by [s]. *)
let max_score s = s.max_score

(* [score s] is the player's current score. *)
let score s = s.score

(* [turns s] is the number of turns the player has taken so far. *)
let turns s = s.turns

(* [current_room_id s] is the id of the room in which the adventurer
 * currently is. *)
let current_room_id s = s.current_room_id

(* [inv s] is the list of item id's in the adventurer's current inventory.
 * No item may appear more than once in the list.  Order is irrelevant. *)
let inv s = List.map (get_item_id) s.inv

(* [visited s] is the list of id's of rooms the adventurer has visited.
 * No room may appear more than once in the list.  Order is irrelevant. *)
let visited s = List.map (get_room_id) s.visited

(* [locations s] is an association list mapping item id's to the
 * id of the room in which they are currently located.  Items
 * in the adventurer's inventory are not located in any room.
 * No item may appear more than once in the list.  The relative order
 * of list elements is irrelevant, but the order of pair components
 * is essential:  it must be [(item id, room id)]. *)
let locations s =
  let tuple_item_room = List.split s.locations in
  let list_items = fst(tuple_item_room) in
  let list_rooms = snd(tuple_item_room) in
  let list_item_ids = List.map (get_item_id) list_items in
  let list_room_ids = List.map (get_room_id) list_rooms in
    List.combine list_item_ids list_room_ids

(* [go_action s obj] is the state resulting from executing the "go"
 * command with [obj] as its argument, acting on state [s] *)
let go_action s obj =
  if obj = "" then raise (InvalidDirection) else
  let room_current = room_from_id s.map.map_rooms s.current_room_id in
  let str_list_exits = List.map (get_exit_direction) room_current.room_exits in
   if (List.exists ((=) obj) str_list_exits) then
      (* if they work, make new state in new room *)
    let exit_obj = get_exit_object obj room_current.room_exits in
    let new_room = room_from_id s.map.map_rooms exit_obj.exit_room_id in
      if List.exists ((=) new_room) s.visited then
        let new_s = {
          max_score = s.max_score;
          score = s.score;
          turns = s.turns + 1;
          current_room_id = new_room.room_id;
          inv = s.inv;
          visited = s.visited;
          locations = s.locations;
          map = s.map
        } in
        let room_c = room_from_id new_s.map.map_rooms new_s.current_room_id in
        let _ = print_endline room_c.room_description in new_s
      else
        let new_s = {
          max_score = s.max_score;
          score = s.score + new_room.room_points;
          turns = s.turns + 1;
          current_room_id = new_room.room_id;
          inv = s.inv;
          visited = new_room :: s.visited;
          locations = s.locations;
          map = s.map
        } in
        let room_c = room_from_id new_s.map.map_rooms new_s.current_room_id in
        let _ = print_endline room_c.room_description in new_s
    else
      raise (InvalidDirection)

(* [take_action s obj] is the state resulting from executing the "take"
 * command with [obj] as its argument, acting on state [s] *)
let take_action s obj =
  if obj = "" then raise (InvalidItem) else
  (* make object into item *)
  let item_object = item_from_id s.map.map_items obj in
  (* check that object is in current room (via locations) *)
  let list_location_items = List.map (get_item_from_location) s.locations in
    if (List.exists ((=) item_object) list_location_items) then
      let location_object = get_location_object item_object s.locations in
      let room_current = room_from_id s.map.map_rooms s.current_room_id in
      if (location_object.room = room_current) then
        (* add object to inventory *)
        let new_inv = item_object :: s.inv in
        (* update locations *)
        let new_locations = List.remove_assoc item_object s.locations in
        (* update score if needed *)
        let _ = print_string "You took " in
        let _ = print_endline obj in
        if (List.exists ((=) obj) room_current.room_treasure) then
          {
            max_score = s.max_score;
            score = s.score - item_object.item_points;
            turns = s.turns + 1;
            current_room_id = s.current_room_id;
            inv = new_inv;
            visited = s.visited;
            locations = new_locations;
            map = s.map
          }
        else {
            max_score = s.max_score;
            score = s.score;
            turns = s.turns + 1;
            current_room_id = s.current_room_id;
            inv = new_inv;
            visited = s.visited;
            locations = new_locations;
            map = s.map
          }
      else raise (InvalidItem)
    else raise (InvalidItem) (* item was not in locations *)

(* [drop_action s obj] is the state resulting from executing the "drop"
 * command with [obj] as its argument, acting on state [s] *)
let drop_action s obj =
  if obj = "" then raise (InvalidItem) else
  (* make object into item *)
  let item_object = item_from_id s.map.map_items obj in
  if (List.exists ((=) item_object) s.inv) then
    (* remove from inv *)
    let new_inv = remove_from_list item_object s.inv in
    (* put into locations (based on current room)*)
    let room_current = room_from_id s.map.map_rooms s.current_room_id in
    let new_locations = (item_object, room_current) :: s.locations in
    (* update score if needed *)
    let _ = print_string "You dropped " in
    let _ = print_endline obj in
    if (List.exists ((=) obj) room_current.room_treasure) then
      {
        max_score = s.max_score;
        score = s.score + item_object.item_points;
        turns = s.turns + 1;
        current_room_id = s.current_room_id;
        inv = new_inv;
        visited = s.visited;
        locations = new_locations;
        map = s.map
      }
    else
      {
        max_score = s.max_score;
        score = s.score;
        turns = s.turns + 1;
        current_room_id = s.current_room_id;
        inv = new_inv;
        visited = s.visited;
        locations = new_locations;
        map = s.map
      }
  else raise (InvalidItem)

(* [look_action s] is [s], before returning [s], the current rooms
 * description is printed for the user *)
let look_action s =
  let room_current = room_from_id s.map.map_rooms s.current_room_id in
  let _ = print_endline room_current.room_description in
    s

(* [inv_action s] is [s], before returning [s], the current inventory
 * list is printed for the user *)
let inv_action s =
  let _ = print_inv_list s.inv in
    s

(* [score_action s] is [s], before returning [s], the current score
 * is printed for the user *)
let score_action s =
  let _ = print_int s.score in
  let _ = print_string "\n" in
    s

(* [turns_action s] is [s], before returning [s], the number of turns
 * is printed for the user *)
let turns_action s =
  let _ = print_int s.turns in
  let _ = print_string "\n" in
    s

(* [quit_action s] raises a quit exception, ending the game *)
let quit_action s =
  raise (Quit)

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
  | Go -> go_action st com.obj
  | Take -> take_action st com.obj
  | Drop -> drop_action st com.obj
  | Look -> look_action st
  | Inventory -> inv_action st
  | Score -> score_action st
  | Turns -> turns_action st
  | Quit -> quit_action st
  (* only transform state *)

(* [repl st] is a unit value that acts as the looping mechanism
 * in the Read Eval Print Loop of the terminal. Uses user input to 
 * execute a command and create a new state,
 * then calls itself again on that state. Exception handling
 * is also implemented, in the case that a user quits the game,
 * or another exception is thrown. *)
let rec repl =
  try
  print_endline "Please enter a command";
  print_string  "> ";
  let input = read_line () in
  let _ = process input in
    repl
  with
  | Illegal -> let _ = print_endline "An error occured" in
    repl
  | InvalidDirection -> let _ = print_endline "Invalid direction entered" in
    repl
  | InvalidItem -> let _ = print_endline "Invalid item entered" in
    repl
  | Quit -> let _ = print_endline "Good bye" in ()

let () =
  ANSITerminal.(print_string [red] 
    "\n\nWelcome to MLB terminal.\n");
  print_endline "What's your name?\n";
  print_string  "> ";
  let name = read_line () in
  	let _ = print_string "Hello" ^ name in 
    repl 