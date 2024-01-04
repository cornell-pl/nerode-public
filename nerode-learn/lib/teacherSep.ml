(** Teacher which starts with a regular language for the positive example set
    and a regular language for the negative example set. *)

open Nerode

type word = Word.t

type t = {
  pos: Dfa.t;
  neg: Dfa.t;
  posrx: Rx.t;
  negrx: Rx.t;
  d_tbl: (word * word, word option) Hashtbl.t;
  qcount: int ref;
}

let make (a: Alphabet.t) (p: Rx.t) (n: Rx.t) =
  let pos = Dfa.of_rx a p in
  let neg = Dfa.of_rx a n in
  if Dfa.is_empty (Dfa.intersect pos neg) then
    { pos = pos; neg = neg; posrx = p; negrx = n; d_tbl = Hashtbl.create 101; qcount = ref 0 }
  else
    failwith "L+ and L- were not disjoint!"

let conjecture (t: t) (c: Dfa.t) =
  let p = Dfa.intersect t.pos (Dfa.complement c) in
  let n = Dfa.intersect t.neg c in
  match Dfa.is_empty p, Dfa.is_empty n with
  | true, true -> None
  | false, _ -> Some (Dfa.rep p)
  | _, false -> Some (Dfa.rep n)

let query (t: t) (w: word) =
  let () = t.qcount := !(t.qcount) + 1 in
  if Dfa.accept t.pos w then
    Some true
  else if Dfa.accept t.neg w then
    Some false
  else
    None

(* [compute_distinguish t s1 s2] returns (Some suffix) if s1@suffix is in pos
   and s2@suffix is neg or s1@suffix is in neg and s2@suffix is in pos; and
   None if no such suffix exists. *)
(*
let compute_distinguish (t: t) (s1: word) (s2: word) : word option =
  let resid_by (w: word) (s: WordSet.t) =
    WordSet.filter_map (Alphabet.resid w) s in
  let pn = WordSet.inter (resid_by s1 t.pos) (resid_by s2 t.neg) in
  let np = WordSet.inter (resid_by s1 t.neg) (resid_by s2 t.pos) in
  match WordSet.union pn np with
  | e when WordSet.is_empty e -> None
  | s -> Some (WordSet.min_elt s)
*)

(* Memoized distinguish query *)
let distinguish_concrete (t: t) (s1: word) (s2: word) : word option =
  failwith "unimplemented"
  (*
  let s_sm, s_lg = if s1 < s2 then s1, s2 else s2, s1 in
  match Hashtbl.find_opt t.d_tbl (s_sm, s_lg) with
  | None -> let r = compute_distinguish t s_sm s_lg in
            let () = Hashtbl.add t.d_tbl (s_sm, s_lg) r in
            r
  | Some r -> r
  *)

let number_queries (t: t) : int = !(t.qcount)

let distinguish _ _ _ _ = failwith "unimplemented"
