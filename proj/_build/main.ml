open Game
open Command
open State

let rec print winners text = 
  print_endline text;
  if (text = "") && (List.length winners = 2)
  then (List.nth winners 0 |> string_of_int) ^ " and " 
       ^ (List.nth winners 1 |> string_of_int)
  else 
    match winners with 
    | [] -> text 
    | id::[] -> print [] (text ^ "and " ^ (id |> string_of_int))
    | id::t -> print t (text ^ (id |> string_of_int) ^ ", ")

let rec end_phase game st = 
  let winner_check = winner_check st in 
  match winner_check with 
  | (winners,points) ->
    let p_text = points |> string_of_int in 
    if List.length winners > 1 
    then let w_text = print (List.rev winners) "" in 
      print_endline ("Players " ^ w_text ^ " have tied with " ^ p_text ^
                     " points! Congratulations!" ); exit 0
    else let w_text = List.hd winners |> string_of_int in 
      print_endline ("Player " ^ w_text ^ " has won the game with " ^
                     p_text ^ " points! Congratulations!"); exit 0 


(** [check_phase game st] is the check phase of [game] with the final state 
    [st], where players check each other's word lists. *)
let rec check_phase game st = 
  print_endline "Turns completed! Entering check phase."; 
  print_endline "If everything looks good, enter 'valid', or
  if any words look wrong, enter 'invalid (the word here)'. "; 
  if current_player st > State.player_count st 
  then (ignore(Sys.command "clear"); end_phase game st)
  else (
    print_endline ("(Player " ^ (State.current_player st |> string_of_int)
                   ^ ") " ^ "Check your next player's word list:");
    State.print_player_word_list st (next_player st);
    print_string "> ";
    (match parse_check (read_line()) with
     | exception Empty -> print_endline "Please enter a command."; 
       check_phase game st
     | exception Malformed -> 
       print_endline "Malformed command. Available commands: 'valid', 'invalid'"; 
       check_phase game st
     | your_command -> (match your_command with
         | Valid -> State.valid game st |> check_phase game
         | Invalid wl -> State.invalid wl game st |> check_phase game))
  )

(** [loopgame game st] is the [game] with updating states [st]. *)
let rec loopgame game st : unit = 
  let turns_left = State.turns st in 
  if turns_left = 0 
  then (
    ignore(Sys.command "clear");
    check_phase game st)
  else (
    let points = State.current_player_points st |> string_of_int in
    print_list game;
    print_endline ("There are " ^ (turns_left |> string_of_int) 
                   ^ " turns left in the game.");
    print_endline ("(Player " ^ (State.current_player st |> string_of_int)
                   ^ "), you currently have " ^ points 
                   ^ " points. Enter your word: ");
    print_string "> ";
    match parse (read_line()) with
    | exception Empty -> print_endline "Please enter a command."; 
      loopgame game st
    | exception Malformed -> 
      print_endline 
        "Malformed command. Available commands: 'create', 'pass', 'quit'"; 
      loopgame game st
    | your_command -> (match your_command with
        | Quit -> print_endline "Bye!"; exit 0
        | Pass -> 
          ignore(Sys.command "clear");
          print_endline ("Player " ^ (State.current_player st |> string_of_int) 
                         ^ " has passed."); 
          begin match pass game st with 
            | Legal st' -> loopgame game st'
            | Illegal -> loopgame game st
          end
        | Create w -> 
          if List.mem_assoc w (State.current_player_wordlist st) 
          then (print_endline "This word has already been created."; 
                loopgame game st) 
          else 
            begin match create w game st with
              | Illegal -> 
                print_endline 
                  "This word cannot be created with your letter set."; 
                loopgame game st
              | Legal st' -> ignore(Sys.command "clear"); loopgame game st'
            end
      )

  )


let rec ask_players() = 
  print_endline "How many players: "; 
  print_string "> "; 
  match parse_number (read_line()) with
  | 0 -> print_endline "ERROR. Enter a valid number of players: "; 
    ask_players()
  | x -> x



(** [play_game j] starts the game with the letter set generated from the 
    alphabet in file [j]. *)
let play_game j = 
  let json = match Yojson.Basic.from_file j with
    | exception Sys_error s -> failwith ("Your input failed. " 
                                         ^ s ^ "\n Start the game over. ")
    | _ -> Yojson.Basic.from_file j
  in 
  let our_game = combo_set (from_json json) in
  let num_players = ask_players() in
  let initst = init_state our_game num_players in 
  loopgame our_game initst

(** [main ()] prompts for the game to play, then starts it. *)
let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to ANAGRAMS.\n");
  print_endline 
    "Please enter the name of the game alphabet file you want to load.\n";
  (** I think we could also automatically load an alphabet file by random,
      or by number of turns, time limit, etc. *)
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()