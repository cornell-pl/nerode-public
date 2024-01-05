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


  let rec kvloop (alpha: Alphabet.t) (teacher: teacher) (ct: ClassTree.t) : Dfa.t =
    let oracle = query teacher in
    let m = ClassTree.hypothesis ct oracle alpha in
    let () = if CliOpt.verbose () then
        begin
          Printf.printf "Conjecture.\n";
          Dfa.print m
        end in
    match Teacher.conjecture teacher m with
    | None -> m
    | Some cex -> let ct' = ClassTree.cex_update ct oracle alpha cex in
                  let () = if CliOpt.verbose () then ClassTree.print ct' alpha in
                  kvloop alpha teacher ct'
  
  let learn (alpha: Alphabet.t) (teacher: teacher) =
    let accept_empty = query teacher Word.epsilon in

    let eq _ = Fun.const true in
    let d _ = Fun.const 0 in
    let e = Fun.const accept_empty in

    (* Either "accept all" machine or "reject all" machine *)
    let init_hyp = Dfa.mk_dfa { eq; d; e } alpha 0 in

    match Teacher.conjecture teacher init_hyp with
    | None -> init_hyp
    | Some cex -> let rej, acc = if accept_empty then
                                    cex, Word.epsilon
                                 else
                                    Word.epsilon, cex in
                  let ct = ClassTree.init rej acc Word.epsilon in
                  let () = if CliOpt.verbose () then ClassTree.print ct alpha in
                  kvloop alpha teacher ct
end
