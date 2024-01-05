(** A word is just a finite sequence of symbols from an Alphabet *)

(** [t] is the representation type for words, aka w *)
type t = Alphabet.symbol list

(** [compare w1 w2] returns a negative int if w1 is lexicographically earlier,
a positive int if later, and 0 if the words are the same.*)
val compare : t -> t -> int

(** [epsilon] is the empty string ε*)
val epsilon : t

(** [prefixes w] returns a list of the prefixes of [w], including epsilon. For example,
    prefixes HELLO would return [[]; H; HE; HEL; HELL; HELLO] *)
val prefixes : t -> t list 

(** [suffixes w] takes a word [w] and returns a list of the suffixes of [w],
including epsilon, with the suffixes being type Word.t. 
For example, suffixes HELLO would return [HELLO; ELLO; LLO; LO; O; []]*)
val suffixes : t -> t list 

(** [append_letter l w] returns the word [w] with the letter [l] attached to the back
E.g., append_letter HELL O = HELLO*)
val append_letter : t -> Alphabet.symbol -> t

(** [concat w1 w2] return the concatenation of [w1] and [w2], i.e. [w1 . w2].
For example, [concat "one" "two"] is "onetwo" *)
val concat : t -> t -> t

(** Creates a word from a list of ints, where each int is a letter in the word,
and the letters are in the same order as the word*)
val of_intlist : int list -> t  

(** Convert a word to a list of ints. *)
val to_intlist : t -> int list

(** Convert a word to its string representation (for output). The alphabet's string
    representation for each symbol is used. *)
val to_string : Alphabet.t -> t -> string

(** Convert to a list of [Alphabet.symbol]. *)
val to_symlist : t -> Alphabet.symbol list

(** Convert from a list of [Alphabet.symbol]. *)
val of_symlist : Alphabet.symbol list -> t

(** [prefix_of w1 w2] returns [true] if [w1 = w2@suffix] for some (possibly
    empty) word [suffix]. *)
val prefix_of : t -> t -> bool

(** [resid pre w] returns [Some s] if [w = pre@s]. Otherwise returns [None]. *)
val resid : t -> t -> t option

(** Form a list of words from a list of strings. Each string in the list corresponds to a
    single alphabet symbol, except that the character 'X' is a wildcard.
    That is, for example, for the alphabet \{0, 1\}, [ws_of_strings alphabet
    [["0"; "X"]] converts to the words [[0;0]] and [[0;1]]. *)
val words_of_strings : Alphabet.t -> string list -> t list
