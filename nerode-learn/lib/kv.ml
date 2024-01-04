(** Implementation of Kearns/Vazirani DFA learning algorithm *)

open Nerode
open Teacher
open ActiveLearner

module TWordSet = Teacher.WordSet

module Make (Teacher : Teacher) : ActiveLearner with type teacher = Teacher.t = struct
  type teacher = Teacher.t
  
  let query teacher (w : Word.t) : bool = 
    match Teacher.query teacher w with
    | None -> failwith "Incomplete teacher not supported!"
    | Some b -> b
  
  let learn (alpha: Alphabet.t) (teacher: teacher) = failwith "TODO"
  
end
