(** A learner in an active learning framework which learns a DFA given access
    to a teacher. *)

open Nerode

module type ActiveLearner = sig
  type teacher
  val learn : Alphabet.t -> teacher -> Dfa.t
end
