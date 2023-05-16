(** Representation of a Deterministic Finite Automaton (DFA) *)

type symbol = Alphabet.symbol
type word = Alphabet.word

(** DFA type. *)
type t

(** Type of the DFA states. *)
type state

(** Set of states. *)
module StateSet : Set.S with type elt = int

(** Provides an interface to mk_dfa:
   @param ['a] is the type of state, where:
   [eq s0 s1] Returns true if s0 and s1 are the same state.
   [d a q]    Returns the derivative of q wrt a; that is, the state we should
              transition to on a from q.
   [e q]      Returns true if q is an accepting state.

   The type ['a] does not need to be the same type as [state] because
   states of type [state] are created and assigned during DFA construction.
   This way [mk_dfa] can be polymorphic over *any* input type ['a regular].

   See [dfa.ml] for examples of use in determinization and implementations of
   intersect, union, and difference*)
type 'a regular = { eq: 'a -> 'a -> bool;
                    d: symbol -> 'a -> 'a;
                    e: 'a -> bool }

(** Construct a DFA by derivation outwards from a start state.
    Generalizes (Owens, Reppy, Turon; JFP 2009) approach for Rx->Dfa conversion. *)
val mk_dfa : 'a regular -> Alphabet.t -> 'a -> t

val compare_states : state -> state -> int

(** Return the number of states of the DFA *)
val size : t -> int

(** Return the alphabet of the DFA. *)
val get_alpha : t -> Alphabet.t

(** Return the start state. *)
val get_start : t -> state

(** [step dfa q x] returns the state the DFA would be in after proceeding by
    symbol [x] from state [q]. *)
val step : t -> state -> symbol -> state

(** Return whether the given state is a final state. *)
val accepting : t -> state -> bool

(** Run a word on the DFA and return whether it is accepted. *)
val accept : t -> word -> bool

(** Return whether a DFA accepts all of the words in the first list and rejects
    all of the words in the second list. Return false if either condition fails
    on any string. *)
val validate : t -> word list -> word list -> bool

(** Return true if and only if the DFA recognizes the empty language. *)
val is_empty : t -> bool

(** Return a word in the language of the DFA. *)
val rep : t -> word

(** Return a DFA that recognizes the complement of the language of the given DFA. *)
val complement : t -> t

(** Write the table for the DFA to stdout. *)
val print : t -> unit

module type Determ  = sig
  module N : Nfa.N
  val determinize : N.t -> t
end

(** This module allows NFAs with any [state] type to determinize to DFA. *)
module Determinizer:
  functor (S : Nfa.State) -> Determ

(** Convert the DFA to an NFA which has the same transitions and accepting states. *)
val to_nfa : t -> IntNfa.t

(** Perform DFA minimization. *)
val minimize : t -> t

(** Construct a DFA from a Regular Expression. An alphabet is required since a Regular Expression
    need not contain all symbols from its alphabet, but a DFA does need to
    define transitions for all symbols. *)
val of_rx : Alphabet.t -> Rx.t -> t

(* Construct a Regular Expression representing the language of the DFA. *)
val to_rx : t -> Rx.t

(* Construct a DFA recognizing the union of the languages of two DFAs. *)
val union : t -> t -> t

(* Construct a DFA recognizing the intersection of the languages of two DFAs. *)
val intersect : t -> t -> t

(* Construct a DFA recognizing the language of the first DFA minus the language of
   the second DFA. *)
val diff : t -> t -> t

(* Construct a DFA recognizing the union of the languages of two DFAs. *)
val symdiff : t -> t -> t

(* Construct a DFA recognizing all strings which are either accepted by both
   DFAs or rejected by both DFAs. *)
val equiv : t -> t -> bool
