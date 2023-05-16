(** A module for Words providing additional operations to those of Alphabet. *)

open Nerode

(** [t] is the representation type for words, aka w *)
type t

(** [compare w1 w2] returns a negative int if w1 is lexicographically earlier,
a positive int if later, and 0 if the words are the same.*)
  val compare : t -> t -> int

(** [epsilon] is the empty string ε*)
val epsilon : t

(** [suffixes w] takes a word [w] and returns a list of the suffixes of [w],
including epsilon, with the suffixes being type Word.t. 
For example, suffixes HELLO would return [HELLO; ELLO; LLO; LO; O; []]*)
val suffixes : t -> t list 

(** [append_letter l w] returns the word [w] with the letter [l] attached to the back
E.g., append_letter HELL O = HELLO*)
val append_letter : t -> Alphabet.symbol -> t

(** [concat w1 w2] return the concatenation of [w1] and [w2], i.e. [w1+w2].
For example, [concat one two] is onetwo*)
val concat : t -> t -> t

(** Creates a word from a list of ints, where each int is a letter in the word,
and the letters are in the same order as the word*)
val of_intlist : int list -> t  

(** Convert a word to a list of ints. *)
val to_intlist : t -> int list

(** Convert a word to its string representation (for output). *)
val to_string : t -> string

(** Convert to a list of [Alphabet.symbol]. *)
val to_symlist : t -> Alphabet.symbol list

(** Convert from a list of [Alphabet.symbol]. *)
val of_symlist : Alphabet.symbol list -> t
