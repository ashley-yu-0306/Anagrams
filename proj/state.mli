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

(** [init_state set] is the initial state of the game. The initial state
    has the combination of letters [set], the turns left in the game, the
    current player, and the list of players. *)
val init_state : Game.t -> t

(** [turns state] is the turns left in game state [state]. *)
val turns: t  -> int

(** [calculate_word_points word set] is the points of [word] based on point
    values in [set]. *)
val calculate_word_points: Command.word -> Game.t -> Game.points

(** [create word game state] is the state [state] updated with the [word] created 
    by the player in [game]. *)
val create: Command.word -> Game.t -> t -> t

(** [current_player state] is the player whose turn is active in state [st]. *)
val current_player: t -> player

