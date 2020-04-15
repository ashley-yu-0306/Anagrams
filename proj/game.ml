open Yojson.Basic.Util

type points = int
type letter = string

type alphabet =  { vowels: (letter * points) list; 
                   consonants: (letter * points) list }

type t = { vowels: (letter * points) list; 
           consonants: (letter * points) list }

(** [points_as_int e] is a letter-points pair pased from json with points as 
    integer.*)
let points_as_int e = (fst e, snd e |> to_int)

let from_json j : alphabet = 
  { vowels = j |> member "vowels" |> to_assoc |> List.map points_as_int;
    consonants = j|> member "consonants" |> to_assoc |> List.map points_as_int;
  }

let letter_set a = failwith "unimplemented"

let rec get_points l set = match set with 
| [] -> failwith "not in letter set" (* can be changed when implementing legal/illegal inputs! *)
| (l', p) :: t -> if l = l' then p else get_points l t