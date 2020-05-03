
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

(** [swap_letter a l set] is the [set] of letters with [l] replaced with
    a letter from [a]. *)
val swap_letter: alphabet -> letter -> t -> t 

(** [combo_set a] is the randomly picked letter set for the game. *)
val combo_set: alphabet -> t

(** [print_list a m] prints out the letters and corresponding points in the combo 
    set with text corresponding to game mode [m].*)
val print_list : t -> int -> unit

(** [get_points set l] is the point that the letter [l] is worth. *)
val get_points: t -> letter -> points

(** [get_letters game acc] is a list of letters in the combo set [game].
    Required: all uppercase. *)
val get_letters: t -> letter list

(** [swap_letter a game letter] is the letter-points pair to be swapped with 
    [letter] selected by the player.*)
val swap_letter: alphabet -> letter -> t -> t

(** [generate_new_set l swappair set] is the new combo set after the letter [l] 
    is swapped to the [swappair]. *)
val generate_new_set: letter -> (letter * points) -> t -> t

(** [char_removal s c] is the list of pairs [s] with pairs whose key corresponds
    to the elements in [c] removed. *)
val char_removal: t -> letter list -> t

(** [add_in_pool game l] is the new pool with a new letter from the player 
    who chose to pass.*)
val add_in_pool: t -> letter -> t
