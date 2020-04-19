(** 
   This module represents the state of an the game as it is being played,
   tracking values like the number of turns left and maintaining the state
   of the player as words are entered and points are assigned.
*)

open Game
open Command

(** The abstract types representing the state of the player and the game. *)
type player
type t
type result = Legal of t | Illegal
type player_id = int

(** [init_state set] is the initial state of the game. The initial state
    has the combination of letters [set], the turns left in the game, the
    current player, and the list of players. *)
val init_state : Game.t -> t

(** [turns state] is the turns left in game state [state]. *)
val turns: t  -> int

(** [calculate_word_points word set] is the points of [word] based on point
    values in [set]. *)
val calculate_word_points: Command.word -> Game.t -> Game.points

(** [create word game state] is the result after the player in [game] attempts
    to create the [word]. *)
val create: Command.word -> Game.t -> t -> result

(** [current_player state] is the player whose turn is active in state [st]. *)
(* val current_player: t -> player *)

(** [current_player state] is the player_id whose turn is active in state [st]. *)
val current_player: t -> int

(** [current_player_wordlist state] is the current word-points list of the 
    current player.*)
val current_player_wordlist: t -> (Command.word * Game.points) list

(** [player_count] is the number of players in the game.*)
val player_count: t -> int

(**[invalid word_lst game state] is the updated state where the next player's 
    words list is checked as valid. *)
val invalid: Command.word list -> Game.t -> t -> t

(** [invalid word_lst game state] is the updated state where the next player's 
    words list is already valid.*)
val valid : Game.t -> t -> t

(** [print_player_word_list state id] prints player[id]'s word list.*)
val print_player_word_list: t -> player_id -> unit
