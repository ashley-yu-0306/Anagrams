open Yojson.Basic.Util

type points = int
type letter = string

type alphabet =  { vowels: (letter * points) list; 
                   consonants: (letter * points) list }

(* type t = { vowels: (letter * points) list; 
           consonants: (letter * points) list } *)
type t = (letter * points) list

type all_letters_in_json = (letter * points) list

(** [points_as_int e] is a letter-points pair pased from json with points as 
    integer.*)
let points_as_int e = (fst e, snd e |> to_int)

let from_json j : alphabet = { 
  vowels = j |> member "vowels" |> to_assoc |> List.map points_as_int;
  consonants = j|> member "consonants" |> to_assoc |> List.map points_as_int;
}

let all_letters a : all_letters_in_json = a.consonants @ a.vowels

(** [rand_l a b] returns a random letter in the list [a], with 
    index between 0 (inclusive) and the bound [b] (exclusive). *)
let rand_l a b = (List.nth a (Random.self_init(); Random.int b))

let combo_set_var a lim =  
  let vow_num = ((lim + 1) / 3) in
  let vow_lst = List.init vow_num (fun x -> rand_l a.vowels 5) in
  let cons_num = lim - vow_num in
  let cons_lst = List.init cons_num (fun x -> if x < 4 then 
                                        rand_l a.consonants 21 
                                      else rand_l a.consonants 18) in
  vow_lst @ cons_lst

(** [to_list a] returns the keys of the association list [a] except for key [k].*)
let rec to_list a k  = 
  match a with 
  | [] -> []
  | (char, point)::t -> if char != k then (char)::to_list t k else to_list t k

let swap_letter a l set = 
  let ul = String.uppercase_ascii l in 
  let new_a_v = to_list a.vowels l in
  let new_a_c = to_list a.consonants l in 
  let new_a = new_a_v @ new_a_c in
  let l' = rand_l new_a (List.length new_a) in 
  let points = try List.assoc l' a.consonants with 
    | Not_found -> List.assoc l' a.vowels in 
  let new_set = List.remove_assoc ul set in 
  ((l',points)::new_set)

let rec print_letters s = 
  match s with 
  | [] -> ()
  | (k,v)::t -> print_endline k; print_letters t

let combo_set a = 
  [(rand_l a.vowels 5);(rand_l a.vowels 5); (rand_l a.consonants 21); 
   (rand_l a.consonants 21); (rand_l a.consonants 21); (rand_l a.consonants 21)]

let generate_new_set l swappair set = swappair :: (List.remove_assoc l set)

let print_list2 a = print_endline "Your Letters: "; 
  List.iter (fun (k,v) -> print_string (k ^ ", worth "); 
              print_int v; print_endline " points. ") a

let print_list a m= if m = 1 then print_endline "\nYour Letters: \n"
  else print_endline "\nThe Pool: \n";
  List.iter (fun (k,v) -> ANSITerminal.(print_string [Bold;blue] ("  " ^ k ^ "     "))) a;
  print_string "\n-----------------------------------------------------------------------------\n";
  List.iter (fun (k,v) -> print_string ""; print_int v; print_string " pts   ") a;
  print_endline "\n"

let rec get_points a l = match a with 
  | [] -> failwith "not in letter set"
  (* can be changed when implementing legal/illegal inputs! *)
  | (l', p) :: t -> if l = l' then p else get_points t l 

let get_letters game = List.map fst game

let rec char_removal s c = 
  match c with 
  | [] -> s 
  | h::t -> char_removal (List.remove_assoc h s) t

let add_in_pool game l a = (l,(get_points a l))::game
(*(l, the letter's points) :: game *)
