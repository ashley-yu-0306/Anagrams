open Command

type player = {   
  player_words:(Command.word * Game.points) list;
  total_points: Game.points;
}

type  t = {
  turns_left: int;
  player_list: (int * player) list
}

let init_state game = failwith "unimplemented"
let turns state = failwith "unimplemented"
let create word game state = failwith "unimplemented"
let current_player state = failwith "unimplemented"
let calculate_word_points word = failwith "unimplemented"