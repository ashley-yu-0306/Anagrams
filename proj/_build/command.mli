
(** The type [word] represents the word that follows the command. *)
type word = string

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a word. *)
type command = 
  | Create of word
  | Quit
  | Pass

(** The type [check] represents a player command when checking other's words 
    list that is decomposed into a verb and possibly a word. *)
type check =
  | Valid
  | Invalid of word

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The next word, if any, become the word.
    Examples: 
    - [parse "    create   word  "] is [Create ["word"]]
    - [parse "quit"] is [Quit]
    - [parse "pass"] is [Pass].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "pass", "quit", nor "create",
    or if the verb is "quit" or "pass" and there is a word following,
    or if the verb is "create" and there is no word following. *)
val parse : string -> command