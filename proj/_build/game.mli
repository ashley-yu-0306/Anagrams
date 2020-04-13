
(** The abstract type of values representing games. *)
type t

(** *)
type points = int

(** *)
type letter = char


(** [from_json j] is the game that [j] represents.
    Requires: [j] is a valid JSON game representation. *)
(* val from_json : Yojson.Basic.t -> t *)