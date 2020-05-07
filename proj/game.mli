
(** The abstract type of values representing games. *)
type t

(** The type of the points that each letter is worth. *)
type points = int

(** The type of the letters. *)
type letter = string

(** [alphabet] is the type of all the letters and their points read from
    the json file, split into vowels and consonants. *)
type alphabet

(** [all_letters_in_json] is the type of all the letters and their points read 
    from the json file. *)
type all_letters_in_json

(** [from_json j] is the game alphabet that [j] represents.
    Requires: [j] is a valid JSON game representation. *)
val from_json : Yojson.Basic.t -> alphabet

(** [combo_set_var a lim] is the randomly picked letter set for the game. *)
val combo_set_var: alphabet -> int -> t

(** [all_letters a] is all the letters and their points read 
    from the json file. *)
val all_letters: alphabet -> all_letters_in_json

(* [set_length s] is the length of the set [s]. *)
val set_length: t -> int 

(** [swap_letter a l set] is the [set] of letters with [l] replaced with
    a letter from [a]. *)
val swap_letter: alphabet -> letter -> t -> t 

(** [print_list a m] prints out the letters and corresponding points in the 
    combo set with text corresponding to game mode [m].*)
val print_list : t -> int -> bool -> unit

(** [get_points set l] is the point that the letter [l] is worth. *)
val get_points: all_letters_in_json -> letter -> points

(** [get_letters game acc] is a list of letters in the combo set [game].
    Required: all uppercase. *)
val get_letters: t -> letter list

(** [generate_new_set l swappair set] is the new combo set after the letter [l] 
    is swapped to the [swappair]. *)
val generate_new_set: letter -> (letter * points) -> t -> t

(** [remove_letter s c] is the list of pairs [s] with pairs whose key 
    corresponds to the elements in [c] removed.
    Precondition: letters in [c] are in [s]. *)
val remove_letter: t -> letter list -> t

(** [add_in_pool game l] is the new pool with a new letter from the player 
    who chose to pass.*)
val add_in_pool: t -> letter -> all_letters_in_json -> t

(** [replenish_pool s n a] is the pool [s] replenished to be length
    [n] with letters in alphabet [a]. *)
val replenish_pool: t -> int -> all_letters_in_json -> t

(** [create_combo_word g] make the combo list a string. *)
val create_combo_word: t -> string

(** [make_a_lst s] makes an API call and generates all possible
    anagrams of the combo set. *)
val make_a_lst: string -> string list Lwt.t