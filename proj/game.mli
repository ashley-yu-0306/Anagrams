
(** The abstract type of values representing games. *)
type t

(** The type of the points that each letter is worth. *)
type points = int

(** The type of the letters. *)
type letter = char

type alphabet

(** [from_json j] is the game that [j] represents.
    Requires: [j] is a valid JSON game representation. *)
val from_json : Yojson.Basic.t -> alphabet