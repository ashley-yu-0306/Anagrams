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

(** [init_player] initializes a player *)
let init_player = {
  player_words = [];
  total_points = 0;
}

let init_state set = {
  turns_left= 10;
  player_list= [(1,init_player);(2,init_player)];
  current_player = 1;
  set= set;
}

let turns state = 
  state.turns_left


let calculate_word_points word set = 
  let seq = word |> String.to_seq |> List.of_seq |> List.map (Game.get_points set) in 
  List.fold_right (+) seq 0

(** [update_player_list state players word id] updates the state of the player with
player_id [id] in [players] by adding points gained in entering [word]. Points are
calculated with the letter combination set in [state].*)
let rec update_player_list state players word id  = 
  match players with
  | [] -> [] 
  | (k,v)::t -> if k = id 
    then let points = calculate_word_points word state.set in 
    let words = List.append (v.player_words) ((word,points)::[]) in 
    let player = {
      player_words = words;
      total_points = v.total_points + points;
    } in (k,player)::(update_player_list state t word id)
    else (k,v)::(update_player_list state t word id)

let create word game state = 
  let player = state.current_player in 
  let player_l = state.player_list in 
  let new_player_l = update_player_list state player_l word player in
   {
    turns_left = state.turns_left - 1;
    player_list = new_player_l;
    current_player = if player = 2 then 1 else player + 1;
    set= state.set;
  } 

let current_player state = 
  state.current_player
