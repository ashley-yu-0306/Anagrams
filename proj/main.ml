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
      ANSITerminal.(print_string [red] ("Players " ^ w_text ^ 
                                        " have tied with " ^ p_text ^ 
                                        " points! Congratulations!\n")); exit 0
    else let w_text = List.hd winners |> string_of_int in 
      ANSITerminal.(print_string [red] ("Player " ^ w_text ^ 
                                        " has won the game with " ^ p_text 
                                        ^ " points! Congratulations!\n")); 
      exit 0 


(** [check_phase game st] is the check phase of [game] with the final state 
    [st], where players check each other's word lists. *)
let rec check_phase game st = 
  print_endline 
    "If everything looks good, enter 'valid', or if any words look wrong, 
      enter 'invalid (the word or words separated with space)'. "; 
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

let each_turn_print st game = 
  (* First, we want to print out the pool: *)
  print_list game 2;
  (* Print all player's current words. *)
  print_all_player_word_list st;
  (* Print each player's letter.  *)
  print_player_letter st

let rec loopgame2 game st json : unit =  
  let turns_left = State.turns st in 
  if turns_left = 0 
  then (
    ignore(Sys.command "clear");
    ANSITerminal.(print_string [green] 
                    "Turns completed! Entering check phase... \n \n"); 
    check_phase game st)
  else (
    let points = State.current_player_points st |> string_of_int in
    let set = State.current_player_letter_set st in 
    each_turn_print st game;
    print_endline ("There are " ^ (turns_left |> string_of_int) 
                   ^ " turns left in the game.");
    print_endline ("(Player " ^ (State.current_player st |> string_of_int)
                   ^ "), you currently have " ^ points 
                   ^ " points. Enter your word: ");
    ANSITerminal.(print_string [yellow] 
                    "Available commands: 'create', 'pass', 'steal', 'quit'.\n");
    print_string "> ";
    match parse (read_line()) with
    | exception Empty -> print_endline "Please enter a command."; 
      loopgame2 game st json
    | exception Malformed -> 
      print_endline 
        "Malformed command. Available commands: 'create', 'pass', 'quit', 'steal'."; 
      loopgame2 game st json
    | your_command ->  (match your_command with
        | Quit -> print_endline "Bye!"; exit 0
        | Pass -> 
          ignore(Sys.command "clear");
          print_endline ("Player " ^ (State.current_player st |> string_of_int) 
                         ^ " has passed.\n"); 
          begin match pass game st with 
            | Legal st' -> loopgame2 game st' json
            | Illegal -> failwith "Impossible error"
          end
        | Create w -> 
          if List.mem_assoc (String.uppercase_ascii w) 
              (State.current_player_wordlist st) 
          then (print_endline "This word has already been created."; 
                loopgame2 game st json) 
          else 
            begin match create w set st with
              | Illegal -> 
                print_endline 
                  "This word cannot be created with your letter set."; 
                loopgame2 game st json
              | Legal st' -> ignore(Sys.command "clear"); loopgame2 game st' json
            end
        | Steal (id, old_word, new_word) -> let target = String.uppercase_ascii old_word in 
          let steal_from = State.get_wordlist_by_id st id in
          if List.mem_assoc target steal_from then 
            match steal target id st with 
            | Illegal -> print_endline "Illegal"; loopgame2 game st json;
            | Legal st' -> 
              loopgame2 game st' json 
          else 
            (print_endline "This letter is not in your letter set. Please try again."; 
             loopgame2 game st json) 
        |_ -> print_endline 
                "Malformed command. Available commands: 'create', 'pass', 'quit', 'swap'."; 
      )

  )



(** [loopgame game st json] is the [game] with updating states [st]. *)
let rec loopgame game st json : unit = 
  let turns_left = State.turns st in 
  if turns_left = 0 
  then (
    ignore(Sys.command "clear");
    ANSITerminal.(print_string [green] 
                    "Turns completed! Entering check phase... \n \n"); 
    check_phase game st)
  else (
    let points = State.current_player_points st |> string_of_int in
    let set = State.current_player_letter_set st in 
    print_list (set) 1;
    print_endline ("There are " ^ (turns_left |> string_of_int) 
                   ^ " turns left in the game.");
    print_endline ("(Player " ^ (State.current_player st |> string_of_int)
                   ^ "), you currently have " ^ points 
                   ^ " points. Enter your word: ");
    ANSITerminal.(print_string [yellow] 
                    "Available commands: 'create', 'pass', 'quit', 'swap'.\n");
    print_string "> ";
    match parse (read_line()) with
    | exception Empty -> print_endline "Please enter a command."; 
      loopgame game st json
    | exception Malformed -> 
      print_endline 
        "Malformed command. Available commands: 'create', 'pass', 'quit', 'swap'."; 
      loopgame game st json
    | your_command ->  (match your_command with
        | Quit -> print_endline "Bye!"; exit 0
        | Pass -> 
          ignore(Sys.command "clear");
          print_endline ("Player " ^ (State.current_player st |> string_of_int) 
                         ^ " has passed."); 
          begin match pass game st with 
            | Legal st' -> loopgame game st' json
            | Illegal -> loopgame game st json
          end
        | Create w -> 
          if List.mem_assoc (String.uppercase_ascii w) 
              (State.current_player_wordlist st) 
          then (print_endline "This word has already been created."; 
                loopgame game st json) 
          else 
            begin match create w set st with
              | Illegal -> 
                print_endline 
                  "This word cannot be created with your letter set."; 
                loopgame game st json
              | Legal st' -> ignore(Sys.command "clear"); loopgame game st' json
            end
        | Swap l -> let target = String.uppercase_ascii l in 
          if List.mem target (set |> Game.get_letters) then 
            match swap l st json with 
            | Illegal -> print_endline "Illegal"; loopgame game st json;
            | Legal st' -> 
              print_endline "Your letter has been swapped. You've lost 5 points.";
              ignore(Unix.sleep 3);
              ignore(Sys.command "clear");
              loopgame game st' json else 
            (print_endline "This letter is not in your letter set. Please try again."; 
             loopgame game st json) 
        |_ -> print_endline 
                "Malformed command. Available commands: 'create', 'pass', 'quit', 'swap'."; 
      )

  )


let rec ask_configure() = 
  print_endline "Would you like to configure your game? (answer yes or no)"; 
  print_string "> "; 
  match read_line() with
  | "yes" -> true
  | "no" -> false
  | _ -> print_endline "ERROR. Enter 'yes' or 'no' "; 
    ask_configure()


(** [ask_players] prompts the player for the number of players, and returns
    that number to the initial state. *)
let rec ask_players() = 
  print_endline "How many players: "; 
  print_string "> "; 
  match parse_number (read_line()) with
  | 0 -> print_endline "ERROR. Enter a valid number of players: "; 
    ask_players()
  | x -> x

let rec ask_num_letters() = 
  print_endline "How many letters (max 10): "; 
  print_string "> "; 
  match parse_number (read_line()) with
  | 0 -> print_endline "ERROR. Enter a valid number: "; 
    ask_num_letters()
  | x -> if x > 10 then 
      ask_num_letters() else x

let rec ask_turns() = 
  print_endline "How many turns: "; 
  print_string "> "; 
  match parse_number (read_line()) with
  | 0 -> print_endline "ERROR. Enter a valid number: "; 
    ask_num_letters()
  | x -> x

let rec ask_mode() = 
  print_endline "Which game mode (normal, pool): "; 
  print_string "> "; 
  match read_line() with
  | "normal" -> "normal"
  | "pool" -> "pool"
  | _ -> print_endline "ERROR. Enter a valid game mode: ";
    ask_mode()


(* let rec ask_check() = 
   print_endline "Which check mode (human, dictionary): "; 
   print_string "> "; 
   match read_line() with
   | "human" -> false
   | "dictionary" -> true
   | _ -> print_endline "ERROR. Enter a valid check mode: ";
    ask_check() *)

(** [play_game j] starts the game with the letter set generated from the 
    alphabet in file [j]. *)
let play_game j = 
  let json = match Yojson.Basic.from_file j with
    | exception Sys_error s -> failwith ("Your input failed. " 
                                         ^ s ^ ". Start the game over. ")
    | _ -> Yojson.Basic.from_file j
  in print_endline "The default settings for the game are: ";
  print_endline "-> 6 letters, 2 players, 5 turns per player, normal mode <-";
  let config = ask_configure() in
  if config then
    let num_words = ask_num_letters() in
    let our_game = combo_set_var (from_json json) num_words in
    let num_players = ask_players() in
    let num_turns = ask_turns() in 
    let game_mode = ask_mode() in
    let initst = init_state our_game num_players num_turns game_mode in 
    ignore(Sys.command "clear");
    if game_mode = "normal" then begin
      ANSITerminal.(print_string [red] 
                      "GAME MODE: NORMAL \n \n"); 
      loopgame our_game initst json 
    end
    else begin
      ANSITerminal.(print_string [red] 
                      "GAME MODE: POOL \n \n"); 
      loopgame2 our_game initst json
    end
  else
    let our_game = combo_set_var (from_json json) 6 in
    let initst = init_state our_game 2 5 "normal" in
    loopgame our_game initst json


(** [main ()] prompts for the game to play, then starts it. *)
let main () = 
  ignore(Sys.command "echo resize -s 30 90");
  ignore(Sys.command "clear");
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
