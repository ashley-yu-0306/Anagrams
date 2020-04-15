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

(** [init_state game] is the initial state of [game]. The initial state
has the combination of letters and the turns left in the game. *)
val init_state : Game.t -> t

(** [turns st] is the turns left in game state [st]. *)
val turns: t  -> int

(** [create com game st] is the state [st] updated with the word created 
by the player in [game] with command [com]. *)
val create: Command.word -> Game.t -> t -> t

(** [current_player st] is the player whose turn is active in state [st]. *)
val current_player: t -> player

(** [calculate_word_points w] is the points of the word [w]. *)
val calculate_word_points: Command.word -> Game.points