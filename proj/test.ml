(** [Test Plan] 
    1) Printing effects, random number generaters and functions in Main.ml are 
    tested manually through play tests.
    The rest of the essential functions in Game.ml, Command.ml and State.ml are 
    tested in OUnit. 

    2) Test cases were developed by black box testing. 

    3) Our tests ensure that the functions work as intended in the 
    documentations, and thus along with rigorous play testing, prove the 
    correctness of our system. *)

open Game
open Command
open State
open OUnit2

(** [pp_string s] pretty-prints string [s]. Helper function from a2. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. Helper function from a2. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"


(*=============== Tests for Game ==============*)
let a1 = from_json (Yojson.Basic.from_file "alphabet1.json")


let make_swap_letter_test
    (name: string)
    (a: alphabet)
    (l: letter)
    (s: Game.t)
    (expected_output: Game.t): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (swap_letter a l s))

let make_generate_new_set 
    (name: string)
    (l: letter)
    (swappair: (letter * points))
    (s: Game.t)
    (expected_output: Game.t): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (generate_new_set l swappair s))



let game_tests = [

]


(*=============== Tests for State ==============*)
(*=============== Tests for Command ==============*)

let make_parse_test
    (name: string)
    (str: string)
    (expected_output: command): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str))

let make_check_test
    (name: string)
    (str: string)
    (expected_output: check): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse_check str))

let command_tests = [
  make_parse_test "quit" "quit" Quit;
  make_parse_test "create" "create cat" (Create "cat");
  make_parse_test "pass" "pass" Pass;
  make_parse_test "swap" "swap u" (Swap "u");
  make_parse_test "steal" "steal 1 cat taco" (Steal (1, "cat", "taco"));
  "Empty: empty" >:: (fun _ -> 
      assert_raises Empty (fun _ -> 
          parse ""));
  "Malformed: create" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> 
          parse "create ca t"));
  "Malformed: steal" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> 
          parse "steal cat"));
  "Malformed: swap" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> 
          parse "swap a u"));
  "Malformed: pass" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> 
          parse "pass a"));
  make_check_test "valid" "valid" Valid;
  make_check_test "invalid" "invalid this is invalid" 
    (Invalid ["this";"is";"invalid"]);
  "Empty: empty" >:: (fun _ -> 
      assert_raises Empty (fun _ -> 
          parse_check ""));
  "Malformed: invalid" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> 
          parse_check "invalid"));
  "Malformed: valid" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> 
          parse_check "valid ok"));
]

let suite =
  "test suite for ANAGRAMS"  >::: List.flatten [  
    game_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite