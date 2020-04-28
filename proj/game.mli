
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

(** [combo_set_var a lim] is the randomly picked letter set for the game. *)
val combo_set_var: alphabet -> int -> t

(** [combo_set a] is the randomly picked letter set for the game. *)
val combo_set: alphabet -> t

(** [print_list a] prints out the letters and corresponding points in the combo 
    set.*)
val print_list : t -> unit

(** [get_points set l] is the point that the letter [l] is worth. *)
val get_points: t -> letter -> points

(** [get_letters game acc] is a list of letters in the combo set [game].
Required: all uppercase. *)
val get_letters: t -> letter list

(** [swap_letter a game letter] is the letter-points pair to be swapped with 
    [letter] selected by the player.*)
val swap_letter: alphabet -> t -> letter -> (letter * points)

(** [generate_new_set l swappair set] is the new combo set after the letter [l] 
is swapped to the [swappair]. *)
val generate_new_set: letter -> (letter * points) -> t -> t

