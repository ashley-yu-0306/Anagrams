open Game
open Command

type player
type t

val init_state : Game.t -> t
val turns: t  -> int
val create: Command.word -> Game.t -> t -> t
val current_player: t -> player
