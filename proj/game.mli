
(** The abstract type of values representing games. *)
type t

(** The type of the points that each letter is worth. *)
type points = int

(** The type of the letters. *)
type letter = string

(** [alphabet] represents all the letters and their points read from
    the json file. *)
type alphabet

(** [from_json j] is the game alphabet that [j] represents.
    Requires: [j] is a valid JSON game representation. *)
val from_json : Yojson.Basic.t -> alphabet

(** [combo_set a] is the randomly picked letter set for the game. *)
val combo_set: alphabet -> t

(** [get_points set l] is the point that the letter [l] is worth. *)
(* <<<<<<< HEAD
   val get_points: letter -> t -> points
   ======= *)

