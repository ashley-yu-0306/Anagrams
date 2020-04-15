type word = string

type command = 
  | Create of word
  | Quit
  | Pass

exception Empty

exception Malformed

(** [remove x acc] removes elements from the given list if [x] equals the 
    element, returning a new list [acc] with elements not equal to [x]. *)
let rec remove x acc = function
  | [] -> acc
  | h::t -> if h = x then remove x acc t else remove x (h::acc) t

let parse str = 
  if String.length str = 0 then raise Empty
  else let lst = List.rev (String.split_on_char ' ' str |> remove "" []) in
    if List.length lst = 0 then raise Empty 
    else if List.nth lst 0 = "create" && List.length lst = 2 
    then Create (List.nth lst 1)
    else if List.nth lst 0 = "quit" && List.length lst = 1 
    then Quit
    else if List.nth lst 0 = "pass" && List.length lst = 1
    then Pass
    else raise Malformed
