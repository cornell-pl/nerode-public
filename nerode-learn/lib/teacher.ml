(** A (possibly incomplete) minimally adequate teacher. (I.e., which may
    return Blank/Don't Care for some membership queries. *)

open Nerode

type word = Word.t

(** A finite set of words. *)
module WordSet = Set.Make(struct
  type t = word
  let compare = Stdlib.compare
end)

(** Module type definition for iMAT Teacher. *)
module type Teacher = sig
  open Nerode
  type t

  (** Membership query. Given a word, return [Some true] if the word is a positive
  example, [Some false] if the word is a negative example, or [None] otherwise. *)
  val query : t -> word -> bool option

  (** Validity query. Given a DFA, return [None] if the DFA is valid/correct, or
      otherwise return an example word that is misclassified by the hypothesis. *)
  val conjecture : t -> Dfa.t -> word option

  (** ``Concrete'' distinguish. Given two words [w1] and [w2], return a suffix
      [e] such that [w1@e] is a positive example and [w2@e] is a negative example
      (or vice-versa) if it exists, otherwise, return [None]. *)
  val distinguish_concrete : t -> word -> word -> word option

  (** Distinguish with blanks. [distinguish teacher w1 w1 eset] returns a
      suffix [e] such that [query teacher (w1@e)] does not equal
      [query teacher (w2@e)] if such a suffix exists and is not already in 
      [eset], or [None] otherwise. Note that one or the other query may be 
      [None] (but not both). *)
  val distinguish : t -> word -> word -> 
    WordSet.t -> word option

  (** Return the number of membership queries that have been asked of the given
      teacher.  *)
  val number_queries : t -> int
end
