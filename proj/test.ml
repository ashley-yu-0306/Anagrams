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

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. Reused from a2.*)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pair_compare p1 p2] compares two key-value pairs [p1] and [p2] to see 
    whether they are equivalent. Reused from a4. *)
let pair_compare p1 p2 = 
  if fst p1 = fst p2 && cmp_set_like_lists (snd p1) (snd p2) then 0 else 
    compare (fst p1) (fst p2)

(** [helper l1 l2] is a helper function that compares two lists 
    [l1] and [l2] to see whether they are equivalent. Reused from a4. *)
let helper l1 l2 = match l1, l2 with
  |[], [] -> true
  |h1 :: t1, h2 :: t2 -> pair_compare h1 h2 = 0
  |_ , _ -> failwith "impossible"

(** [cmp_set_like_assoc lst1 lst2] compares two assoc lists to see whether
    they are equivalent set-like lists. Reused from a4. *)
let cmp_set_like_assoc lst1 lst2 =
  let uniq1 = List.sort_uniq pair_compare lst1 in
  let uniq2 = List.sort_uniq pair_compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  helper uniq1 uniq2
(* 
(*=============== Example alphabet ===============*)
(** [alp] is the example alphabet, with only 2 vowels and 4 consonants in 
    json. *)
let alp = from_json (Yojson.Basic.from_file "test_alphabet.json")
(** [set] is the example combo set. [alp] is used with number of letters [lim] =
    6 to eliminate randomization when generating the combo, such that the [set] 
    conatins all the alphabet. Compatablity with randomization will be 
    thoroughly tested in play tests. *)
let set = combo_set_var alp 6

(** [all] is the example alphabet without differentiation between vowels and 
    consonants.*)
let all = all_letters alp *)

(*=============== Tests for Game ==============*)
let alp = from_json (Yojson.Basic.from_file "alphabet1.json")
let set = combo_set_var alp 6
let full_subset_of_set = (get_letters set)
let subset_of_set1 = 
  (List.nth (get_letters set) 0)::((List.nth (get_letters set) 3)::[])
let all = all_letters alp
let set_str = create_combo_word set

let game_tests = [
  "Set length = 6" >:: (fun _ -> 
      assert_equal 6 (set_length set));
  "A: 1" >:: (fun _ -> 
      assert_equal 1 (get_points all "A"));
  "F: 3" >:: (fun _ -> 
      assert_equal 3 (get_points all "F"));

  "get_letters produces list of strings of length 1" >:: 
  (fun _ -> assert_equal 6 
      (List.fold_left (fun a k -> if String.length k = 1 then 1 + a else a) 
         0 (get_letters set)));

  "create_combo_word produces string of set's size" >:: 
  (fun _ -> assert_equal 6 (String.length (create_combo_word set)));

  "remove_letter test 1: removes all letters from game when
   list inputted is all the letters in the set" >:: 
  (fun _ -> assert_equal 0 (set_length (remove_letter set full_subset_of_set)));

  "remove_letter test 2: removes 2 from game when
   list inputted is all 2 of letters in the set" >:: 
  (fun _ -> assert_equal 4 (set_length (remove_letter set subset_of_set1)));

  "add_in_pool: adds letter to pool,
   increasing length of set by 1" >:: 
  (fun _ -> assert_equal 7 (set_length (add_in_pool set "A" all)));

  "make_a_lst: makes an API call to Anagramatica, generating a promise
    containing a string list of the possible words. " >::
  (fun _ -> assert_equal true 
      (if (List.length (Lwt_main.run (make_a_lst set_str))) > 0 
       then true else false));

  "generate_new_set: swaps letter out with a new letter, changing the game" >::
  (fun _ -> assert_equal 6 
      (generate_new_set (List.nth full_subset_of_set 0) ("E",2) set 
       |> set_length));

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