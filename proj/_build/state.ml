open Command


type player = {   
  player_words:(Command.word * Game.points) list;
  total_points: Game.points;
}

type player_id = int

type  t = {
  turns_left: int;
  player_list: (player_id  * player) list;
  current_player: player_id;
  set: Game.t;
}

type result = Legal of t | Illegal

(** [init_player] initializes a player *)
let init_player = {
  player_words = [];
  total_points = 0;
}

let init_state set = {
  turns_left= 4;
  player_list= [(1,init_player);(2,init_player)];
  current_player = 1;
  set= set;
}

let turns state = 
  state.turns_left

(**[word_to_cl n] is a char list of the input [word].*)
let word_to_cl n = List.init (String.length n) (String.get n)

(**[cl_to_ll cl] is a string list of char list [cl], with all letters in 
   uppercase.*)
let cl_to_ll cl = List.map (fun x -> Char.escaped x) cl 
                  |> List.map String.uppercase_ascii


(* let calculate_word_points word set = 
   let seq = word |> String.to_seq |> List.of_seq |> List.map (Game.get_points set) in 
   List.fold_right (+) seq 0 *)
let calculate_word_points word set = List.fold_left 
    (fun x y -> x + Game.get_points set y) 0 (word |> word_to_cl |> cl_to_ll)

(** [update_player_list state players word id] updates the state of the player with
    player_id [id] in [players] by adding points gained in entering [word]. Points are
    calculated with the letter combination set in [state].*)
let rec update_player_list state players word id  = 
  match players with
  | [] -> [] 
  | (k,v)::t -> if k = id 
    then let points = calculate_word_points word state.set in 
      let words = List.append (v.player_words) [(word,points)] in 
      let player = {
        player_words = words;
        total_points = v.total_points + points;
      } in (k,player)::(update_player_list state t word id)
    else (k,v)::(update_player_list state t word id)

(**[remove l lst acc] is [lst] with the first occurance of [l] removed. *)
let rec remove l lst acc = match lst with
  | [] -> acc
  | h :: t -> if l = h then acc @ t else remove l t (h :: acc)

(**[check_illegal ll combo_l] is [true] iff [ll] contains letter(s) that is not
   in the combo or more occurances of some letter offered in the combo. *)
let rec check_illegal ll combo_l = 
  match ll with 
  | [] -> false
  | h :: t -> if not (List.mem h combo_l) then true 
    else check_illegal t (remove h combo_l [])


let create word game state = 
  if word = "" || check_illegal (word |> word_to_cl |> cl_to_ll) 
       (Game.get_letters game) then Illegal
  else
    let player = state.current_player in 
    let player_l = state.player_list in 
    let new_player_l = update_player_list state player_l word player in
    Legal {
      turns_left = state.turns_left - 1;
      player_list = new_player_l;
      current_player = if player = 2 then 1 else player + 1;
      set= state.set;
    } 

let current_player state = 
  state.current_player

let current_player_wordlist state =  
  (List.assoc state.current_player state.player_list).player_words