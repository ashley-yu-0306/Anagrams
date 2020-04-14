type word = string

type command = 
  | Create of word
  | Quit
  | Pass

exception Empty

exception Malformed

let parse str = failwith "unimplemented"