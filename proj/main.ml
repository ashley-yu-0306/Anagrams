(** [play_game j] starts the game with the letter set generated from the 
    alphabet in file [j]. *)
let play_game j = failwith "unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to ANAGRAM.\n");
  print_endline 
    "Please enter the name of the game alphabet file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()