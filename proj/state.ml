open Command
open Game

type player = {   
  player_words:(Command.word * Game.points) list;
  total_points: Game.points;
  player_letter_set: Game.t;
  current_letter: string
}

type player_id = int

type  t = {
  turns_left: int;
  player_list: (player_id  * player) list;
  current_player: player_id;
  total_players: int;
  mode: string;
  set: Game.t;
}

type result = Legal of t | Illegal

let random_letter() = 
  Char.escaped (Char.chr ((Random.self_init(); Random.int 26) + 65))

(** [init_player] initializes a player *)
let init_player set = {
  player_words = [];
  total_points = 0;
  player_letter_set = set;
  current_letter = random_letter()
}

let init_state set num turn mode = {
  turns_left= turn * num; 
  player_list = List.init num (fun i -> ((i + 1), init_player set));
  current_player = 1;
  total_players = num;
  mode = mode;
  set= set;
}

let turns state = 
  state.turns_left

let current_player state = 
  state.current_player

let current_player_wordlist state =  
  (List.assoc state.current_player state.player_list).player_words

let current_player_points state = 
  (List.assoc state.current_player state.player_list).total_points

let current_player_letter_set state =
  (List.assoc state.current_player state.player_list).player_letter_set

let next_player state = 
  if (not (state.current_player = state.total_players))
  then state.current_player + 1 
  else 1

(**[word_to_cl n] is a char list of the input [word].*)
let word_to_cl n = List.init (String.length n) (String.get n)

(**[cl_to_ll cl] is a string list of char list [cl], with all letters in 
   uppercase.*)
let cl_to_ll cl = List.map (fun x -> Char.escaped x) cl 
                  |> List.map String.uppercase_ascii

(* let calculate_word_points word set = 
   let seq = word |> String.to_seq |> List.of_seq |> List.map (Game.get_points set) in 
   List.fold_right (+) seq 0 *)
let calculate_word_points word st = 
  let set = current_player_letter_set st in
  let base = List.fold_left 
      (fun x y -> x + Game.get_points set y) 0 (word |> word_to_cl |> cl_to_ll) in 
  let length = String.length word in 
  if length >= 3 && length < 5 
  then base |> float_of_int |> (fun x -> x*. 1.2) |> int_of_float
  else if length >= 5 
  then base |> float_of_int |> (fun x -> x*. 1.5) |> int_of_float
  else base

(** [update_player_list state players word id] updates the state of the player
    with player_id [id] in [players] by adding points gained in entering [word]. 
    Points are calculated with the letter combination set in [state].*)
let rec update_player_list state players word id  = 
  match players with
  | [] -> [] 
  | (k,v)::t -> if k = id 
    then let points = calculate_word_points word state in 
      let words = List.append (v.player_words) 
          [(String.uppercase_ascii word,points)] in 
      let player = {
        player_words = words;
        total_points = v.total_points + points;
        player_letter_set = v.player_letter_set;
        current_letter = random_letter()
      } in (k,player)::(update_player_list state t word id)
    else (k,v)::(update_player_list state t word id)

(**[remove x lst acc] is [lst] with the first occurance of [x] removed. *)
let rec remove x lst acc = match lst with
  | [] -> acc
  | h::t -> if h = x then remove x t acc else remove x t (h::acc)

(**[check_illegal ll combo_l] is [true] iff [ll] contains letter(s) that is not
   in the combo or more occurances of some letter offered in the combo. *)
let rec check_illegal ll combo_l = 
  match ll with 
  | [] -> false
  | h :: t -> if not (List.mem h combo_l) then true 
    else check_illegal t (remove h combo_l [])


let check_letter_used st word = String.contains word
    ((String.get (List.assoc st.current_player st.player_list).current_letter) 0)

(** [string_to_string_list s i] is the string list of [s], where [i] is the
    length of the string subtracted by 1. *)
let rec string_to_string_list s i = 
  if i>(-1) then String.make 1 (String.get s i) ::string_to_string_list s (i-1) else [] 

let create word state = 
  if word = "" || check_illegal (word |> word_to_cl |> cl_to_ll) 
       (Game.get_letters (current_player_letter_set state)) then Illegal
  else
    let player = state.current_player in 
    let player_l = state.player_list in 
    let new_player_l = update_player_list state player_l word player in
    Legal {
      turns_left = state.turns_left - 1;
      player_list = new_player_l;
      current_player = next_player state;
      total_players = state.total_players;
      mode = state.mode;
      set = state.set;
    } 

let create_p word state = 
  if word = "" || not(check_letter_used state word) || 
     check_illegal (word |> word_to_cl |> cl_to_ll) 
       (Game.get_letters (current_player_letter_set state)) then Illegal
  else
    let player = state.current_player in 
    let player_l = state.player_list in 
    let new_player_l = update_player_list state player_l word player in
    let char_l = string_to_string_list word ((String.length word)-1) in
    Legal { state with
            turns_left = state.turns_left - 1;
            player_list = new_player_l;
            current_player = next_player state;
            set = char_removal state.set char_l
          } 

let pass game state = 
  Legal { state with
          turns_left = state.turns_left - 1;
          current_player = next_player state;
        }

(** [update_player_list3 players ns id] is the list of [players] with 
    the player whose id is [id] updated with a new letter set [ns].*)
let rec update_player_list3 players ns id = 
  let cur_let = (List.assoc id players).current_letter in
  match players with
  | [] -> []
  | (k,v)::t -> if k = id 
    then 
      let player = { v with
                     total_points = v.total_points - 5;
                     player_letter_set = ns;
                     current_letter = cur_let
                   } in (k,player)::(update_player_list3 t ns id)
    else (k,v)::(update_player_list3 t ns id)

let swap l state json = 
  let alphabet = from_json json in 
  let id = state.current_player in 
  let set = current_player_letter_set state in
  let new_set = swap_letter alphabet l set in
  Legal { state with
          turns_left = state.turns_left - 1;
          player_list = update_player_list3 state.player_list new_set id;
          current_player = next_player state;
        }

(** [update_player_list4 players p id] is [players] with [p] instead of the
    player whose id is [id]. *)
let rec update_player_list4 players p id = 
  match players with 
  | [] -> []
  | (k,v)::t -> if k=id then (k,p)::(update_player_list4 t p id) 
    else (k,v)::(update_player_list4 t p id)

let steal w p st = 
  let player_list = st.player_list in 
  let player = List.assoc p player_list in 
  let words = player.player_words in 
  if not (List.mem_assoc (String.uppercase_ascii w) (words)) then Illegal 
  else let player' = { player with player_words = List.remove_assoc w words;} in 
    Legal {st with player_list = update_player_list4 player_list player' p}

let player_count state = 
  state.total_players

let rec winner_check_helper players winners winner_p = 
  match players with 
  | [] -> (winners,winner_p)
  | (id,p)::t -> if p.total_points > winner_p 
    then winner_check_helper t (id::[]) p.total_points
    else if p.total_points = winner_p && (not (List.mem id winners))
    then winner_check_helper t (id::winners) winner_p 
    else winner_check_helper t winners winner_p 

let winner_check state =
  let state' = {state with current_player = 1;} in
  let p_list = state.player_list in 
  let win_id = state'.current_player in 
  let win_p = (List.assoc win_id p_list).total_points in
  winner_check_helper p_list (win_id::[]) win_p

(** =====Below is for check phase====== *)


(** [remove_invalid next_player inv_words state] is a player with all invalid 
    words removed from his words list*)
let rec remove_invalid next_player inv_words state = 
  match inv_words with
  | [] -> next_player
  | h :: t -> (if List.mem_assoc h (next_player.player_words) 
               then (let new_next_pwlst = 
                       List.remove_assoc h (next_player.player_words) in 
                     remove_invalid ({player_words = new_next_pwlst; 
                                      total_points = 
                                        next_player.total_points - 
                                        calculate_word_points h state;
                                      player_letter_set = next_player.player_letter_set;
                                      current_letter = ""}) 
                       t state)
               else remove_invalid next_player t state)

(** [update_player_list2 state word_lst id ] is the new player list with player 
    [id]'s words list checked as valid.*)
let update_player_list2 state word_lst id = 
  let new_next_player = 
    remove_invalid (List.assoc id state.player_list) word_lst state in 
  (id, new_next_player) :: (List.remove_assoc id state.player_list)


let invalid word_lst game state =
  let new_player_l = 
    update_player_list2 state (List.map String.uppercase_ascii word_lst) 
      (next_player state) in
  { state with
    turns_left = 0;
    player_list = new_player_l;
  } 

let valid game state = 
  {state with current_player = state.current_player + 1}

let print_player_word_list state id = 
  let wl = (List.assoc id state.player_list).player_words in
  if wl = [] then print_string "No words yet.\n" else
    List.iter (fun (k,v)-> print_string k; print_newline ();) wl

let print_player_letter st = 
  print_endline ("Current player's letter: " ^ 
                 (List.assoc st.current_player st.player_list).current_letter)

(** [print_all_player_word_list_helper st acc] is a helper function 
    that prints all player[id]'s word list. *)
let rec print_all_player_word_list_helper st acc : unit = 
  if (acc > List.length st.player_list) 
  then ()
  else begin print_string ("Player " ^ string_of_int acc ^ ": "); 
    print_player_word_list st acc;
    print_all_player_word_list_helper st (acc + 1) end

let print_all_player_word_list st = print_all_player_word_list_helper st 1
(* 
let get_wordlist_by_id st id = (List.assoc id st.player_list).player_words *)
