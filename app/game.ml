open Cohttp
open Cohttp_lwt_unix
open Lwt
open Yojson.Basic.Util
open Unix

type date = {
	day : int;
	month : int;
	year : int;
}

type game = {
  home_team : string;
  away_team : string;
  home_score : int;
  away_score : int;
  date_played : date;
  inning : string;
}


let rec get_codes acc games  = match games with
  | [] -> let _ = print_endline "done" in acc 
  | h :: t -> 
    let home_code = h |> member "home_team_id" |> to_string in 
    let away_code = h |> member "away_team_id" |> to_string in  
    let home_name = h |> member "home_team_name" |> to_string in
    let away_name = h |> member "away_team_name" |> to_string in 
    let _ = print_endline (home_name ^ ": " ^  home_code) in 
    let _ = print_endline (away_name ^ ": " ^  away_code) in 
     get_codes home_code t 

let rec get_team_game team games = match games with 
  | [] -> "no game found for " ^ team
  | h::t -> 
    let home = h |> member "home_team_id" |> to_string in 
    let away = h |> member "away_team_id" |> to_string in 
      if home = team then 
        let home_team_runs = h |> member "home_team_runs" |> to_string in
        let away_team_runs = h |> member "away_team_runs" |> to_string in 
        let home_team_name = h |> member "home_team_name" |> to_string in
        let away_team_name = h |> member "away_team_name" |> to_string in 
        home_team_name ^ ": " ^  home_team_runs ^ ", " ^  away_team_name ^ ": " ^  away_team_runs
      else if away = team then  
        let home_team_runs = h |> member "home_team_runs" |> to_string in
        let away_team_runs = h |> member "away_team_runs" |> to_string in
        let home_team_name = h |> member "home_team_name" |> to_string in
        let away_team_name = h |> member "away_team_name" |> to_string in 
        home_team_name ^ ": " ^  home_team_runs ^ ", " ^  away_team_name ^ ": " ^  away_team_runs
		  else get_team_game team t 

let parse_games body team = 
	let json = Yojson.Basic.from_string body in
	let data = json |> member "data" in
	let games_obj = data |> member "games" in 
	let games = games_obj |> member "game" |> to_list in 
  try get_team_game team games with
    | _ -> "no games today"

let fix_day day = 
  if day < 10 then ("0" ^ string_of_int day)
  else string_of_int day 

let get_todays_game team =
  let t = time () |> localtime  in 
  let year = string_of_int (1900 + t.tm_year) in
  let month = fix_day (t.tm_mon + 1) in
  let day = string_of_int t.tm_mday in
  let u = Uri.of_string (
  	"http://mlb.mlb.com/gdcross/components/game/mlb/year_" 
  	^ year ^ "/month_" ^ month ^ "/day_" ^ day ^ "/miniscoreboard.json") in
  Client.get u >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code = 200 then 
      body |> Cohttp_lwt_body.to_string >|= fun body_s ->
        let game_data = parse_games body_s team in 
        game_data
    else failwith "error"

(*let () = 
  let body = Lwt_main.run (get_todays_game "115") in
  print_endline (body)*)
