(** A classification tree closely following the description due to Kearns and
    Vazirani in the book "An Introduction to Computational Learning Theory"
    (1994).  *)

open Nerode

type t

(** [init rej acc suf] returns an initial classification tree *)
val init : Word.t -> Word.t -> Word.t -> t

(** [print alpha ct] prints [ct] to the screen using [alpha] for symbol
    representations. *)
val print : t -> Alphabet.t -> unit

(** [sift ct oracle s] returns the accessor string in the leaf that results from
    sifting [s] down the tree [ct], posing membership queries to [oracle] *)
val sift : t -> (Word.t -> bool) -> Word.t -> Word.t

(** [hypothesis ct oracle] constructs a DFA for [ct], posing membership queries 
    along the way using the provided oracle function *)
val hypothesis : t -> (Word.t -> bool) -> Alphabet.t -> Dfa.t

(** [cex_update ct oracle cex] returns a new classification tree which has been
    updated to account for the word [cex]. It is an error to call this function
    with a word [cex] which is correctly classified by the DFA returned by
    [hypothesis ct oracle alpha]. *)
val cex_update : t -> (Word.t -> bool) -> Alphabet.t -> Word.t -> t
