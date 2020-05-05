(** 
   This module represents the state of an the game as it is being played,
   tracking values like the number of turns left and maintaining the state
   of the player as words are entered and points are assigned.
*)

open Game
open Command

(** The abstract type of the player. *)
type player

(** The abstract type of the game state. *)
type t

(** The type of returning result of a command action. *)
type result = Legal of t | Illegal of string

(** The type of a player id. *)
type player_id = int

(** [init_state set num turn mode] is the initial state of the game. The initial
    state has the combination of letters [set], the turns left in the game, the
    current player, and the list of players. *)
val init_state : Game.t -> int -> int -> string -> Game.all_letters_in_json -> t

(** [turns state] is the turns left in game state [state]. *)
val turns: t  -> int

(** [current_player state] is the player_id whose turn is active in state 
    [st].*)
val current_player: t -> int

val current_player_stolen: t -> (player_id * Command.word) list

(** [current_player_wordlist state] is the current word-points list of the 
    current player.*)
val current_player_wordlist: t -> (Command.word * Game.points) list

(** [current_player_points state] is the total points of the current player in
    [state]. *)
val current_player_points: t -> Game.points

(** [current_player_letter_set state] is the current player's letter set. *)
val current_player_letter_set: t -> Game.t

(** [get_pool st] is the current pool. *)
val get_pool: t -> Game.t

(** [next_player state] gives the [id] of the player whose turn is next. *)
val next_player: t -> player_id

(** [calculate_word_points word set] is the points of [word] based on point
    values in [set]. *)
val calculate_word_points: Command.word ->t -> Game.points

(** [start_message st] prints available messages for the current player. *)
val start_message: t -> unit

(** [create word state s] is the result after the player in [game] attempts
    to create the [word]. [s] is whether or not the word was stolen. *)
val create: Command.word -> t -> bool -> result

(** [pass state] is the result after the player in [game] passes their turn.*)
val pass: t -> result

(** [swap game state] is the result after the player in [game] swaps their 
    letter [l]. *)
val swap: Game.letter -> t -> Yojson.Basic.t -> result

(** [steal w nw p st] is the result after the current player steals word [w] from 
    player [p] to construct word [nw]. *)
val steal: Command.word -> Command.word -> player_id -> t -> result

(** [player_count state] is the number of players in the game.*)
val player_count: t -> int

(** [player_count st] prints the players current letter. *)
val print_player_letter: t -> unit

(** [winner_check state] is the list of winners in game of state [state] and
    the highest number of point achieved by a player in that game.*)
val winner_check: t -> (player_id list * Game.points)

(**[invalid word_lst game state] is the updated state where the next player's 
    words list is checked as valid. *)
val invalid: Command.word list -> Game.t -> t -> t

(** [invalid word_lst game state] is the updated state where the next player's 
    words list is already valid.*)
val valid : Game.t -> t -> t

(** [print_player_word_list state id] prints player[id]'s word list.*)
val print_player_word_list: t -> player_id -> unit

(** [print_all_player_word_list state id] prints all player[id]'s word list.*)
val print_all_player_word_list: t -> unit
