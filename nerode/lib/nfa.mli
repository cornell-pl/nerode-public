(** Representation of a Nondeterministic Finite Automaton (NFA), functorized
    over an abstract type for states. *)

(** Use the Alphabet symbol type. *)
type symbol = Alphabet.symbol

(** Use the Alphabet word type. *)
type word = Word.t

(** Transitions are labeled by a symbol or epsilon *)
type nsymbol = Char of symbol | Eps

module type State = sig
  type t
  module StateSet : Set.S with type elt = t
  val compare : t -> t -> int
  val to_string : t -> string
  val fresh : StateSet.t -> t
end

module type N = sig
 
  type state

  module StateSet : Set.S with type elt = state
  module StateMap : Map.S with type key = state
  module CharMap : Map.S with type key = nsymbol

  type t

  (* Construct an NFA given an alphabet, set of start states, set of final
     states, and a list of transitions. The set of all states is the collection of those
     referenced by the start and final sets and the list of transitions. *)
  val mk_nfa : Alphabet.t -> state list -> state list -> (state*nsymbol*state) list -> t
  val get_alpha : t -> Alphabet.t
  val get_start : t -> StateSet.t
  val contains_final : t -> StateSet.t -> bool

  (* Run the NFA on a word and return [true] if the word is accepted and
     [false] otherwise *)
  val accept : t -> word -> bool

  (* Given an NFA, a set of states, and a symbol, return the set of states that would
     be obtained by performing ``one step'', i.e. reading the given symbol. *)
  val next : t -> StateSet.t -> symbol -> StateSet.t

  (** Return the transitions of the NFA as a list of [state, symbol, state] triples. *)
  val trans_list : t -> (state * nsymbol * state) list

  (** Construct an NFA which recognizes the reverse of the language of the given NFA, i.e. the
      set of words which are the reverse of a word accepted by the input NFA. *)
  val reverse : t -> t

  (** Write a string representation of the NFA to stdout. *)
  val print : t -> unit

  (** Conver the NFA to a Regular Expression. *)
  val to_rx : t -> Rx.t

end

module Make: 
  functor (S : State) -> N with type state = S.t
