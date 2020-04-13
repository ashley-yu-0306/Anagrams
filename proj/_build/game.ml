type points = int
type letter = char

(** [alphabet] represents all the letters and their points read from
    the json file. .*)
type alphabet =  (letter * points) list

(** *)
type t = (letter * points) list


let from_json j = failwith "unimplemented"