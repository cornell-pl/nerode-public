(** A map from prefix closed sets of Words. The operations that fill in values
    of the map do so with a [query] callback function of type [word -> 'a]. (The
    essential example of this is the teacher's membership query, in which case
    ['a] is likely to be [bool] or [bool option]. The functions that include
    [extend] in the name add a leaf for each symbol in the alphabet beyond the
    given word. *)

open Nerode

type symbol = Alphabet.symbol
type word = Word.t

(* ['a t] is a map from a prefix-closed set of words into 'a.
   When words are added, the caller provides a callback (of type word -> 'a)
   for filling in values for keys that were not already in the trie. *)
type 'a t

module SymMap : Map.S with type key = symbol

(** Return an empty trie. *)
val empty : 'a t

(** Return a trie with only the key [[]], which maps to the given value. *)
val eps : 'a -> 'a t

(** Print a string representation of the trie to stdout. *)
val print : 'a t -> Alphabet.t -> ('a -> string) -> unit

(** Make a new trie with [[]] as the only internal node, and a leaf for each
    symbol in the given alphabet. *)
val eps_extend : (word -> 'a) -> Alphabet.t -> 'a t

(** Look up the given word in the given trie. If the word is not present,
    [failwith "Not found!"]. *)
val find : 'a t -> word -> 'a

(** Look up the word in the given trie, returning None if the word is not
    present. *)
val find_opt : (word -> 'a -> bool) -> 'a t -> (word*'a) option

(** Return a new trie with the given function mapped over the values. *)
val map : (word -> 'a -> 'b) -> 'a t -> 'b t

(** Construct a trie whose keys are the prefixes (inclusive) of the given word. 
    Values are assigned using the provided callback function. *)
val of_word : word -> (word -> 'a) -> 'a t

(** Add the prefixes of the given word to the trie, and return the resulting trie.
    Values are assigned using the provided callback function. *)
val add : (word -> 'a) -> 'a t -> word -> 'a t

(** Add the prefixes of the given word, extended by each symbol of the alphabet,
    and return the restulting trie. Values are assigned using the provided
    callback function. *)
val add_extend : (word -> 'a) -> Alphabet.t ->  'a t -> word -> 'a t

(** Construct a trie from the prefixes of a word, extended by each symbol of
    the alphabet. Example: For the alphabet [a =\{a, b\}] and word [w = 'ab']
    [of_word_extend w a query] would add the keys [\{a, b, aa, ab, aba, abb\}]. *)
val of_word_extend : word -> Alphabet.t -> (word -> 'a) -> 'a t

(** Construct a trie from the prefixes of a whole list of words. *)
val of_wordlist : (word -> 'a) -> word list -> 'a t

(** Construct a trie from the prefixes of a whole list of words, extended by
    each symbol of the alphabet. *)
val of_wordlist_extend : (word -> 'a) -> Alphabet.t -> word list -> 'a t

(** Return the keys of the trie. *)
val keys : 'a t -> word list

(** Return the keys of non-leaf nodes of the trie. *)
val node_keys : 'a t -> word list

(** Return the keys of the leaves of the trie. *)
val leaf_keys : 'a t -> word list

(** Return the key-value pairs of the non-leaf nodes of the trie. *)
val node_bindings : 'a t -> (word * 'a) list

(** Return the key-value pairs of the leaves of the trie. *)
val leaf_bindings : 'a t -> (word * 'a) list

(**/**)
val fold : ('a -> word -> 'a) -> 'a -> 'b t -> 'a
val iter : (word -> unit) -> 'a t -> unit
