open Yojson.Basic.Util

type points = int
type letter = string

type alphabet =  { vowels: (letter * points) list; 
                   consonants: (letter * points) list }

(* type t = { vowels: (letter * points) list; 
           consonants: (letter * points) list } *)

type t = (letter * points) list

(** [points_as_int e] is a letter-points pair pased from json with points as 
    integer.*)
let points_as_int e = (fst e, snd e |> to_int)

let from_json j : alphabet = { 
  vowels = j |> member "vowels" |> to_assoc |> List.map points_as_int;
  consonants = j|> member "consonants" |> to_assoc |> List.map points_as_int;
}

(** [rand_l a b] returns a random letter in the list [a], with 
    index between 0 (inclusive) and the bound [b] (exclusive). *)
let rand_l a b = (List.nth a (Random.int b))

let combo_set a = 
  [(rand_l a.vowels 5);(rand_l a.vowels 5); (rand_l a.consonants 21); 
   (rand_l a.consonants 21); (rand_l a.consonants 21); (rand_l a.consonants 21)]


let rec get_points set l = match set with 
  | [] -> failwith "not in letter set"
  (* can be changed when implementing legal/illegal inputs! *)
<<<<<<< HEAD
  | (l', p) :: t -> if l = l' then p else get_points t l 
=======
  | (l', p) :: t -> if l = l' then p else get_points t l
>>>>>>> fc8b14695fdf2ab5d51eb863e1ae84743f5acb17
