(** A passive learner; that is, a learner that starts with a finite list of
    positive examples and negative examples. *)

open Nerode
type word = Word.t

module type HybridLearner = sig
  val learn : Alphabet.t -> word list -> word list -> Dfa.t
end
