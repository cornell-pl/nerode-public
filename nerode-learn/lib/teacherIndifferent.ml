(** Main implementation of the _incomplete_ minimally adequate teacher. *)

open Nerode

type word = Teacher.word

module WordSet = Teacher.WordSet

type t = {
  pos: WordSet.t;
  neg: WordSet.t;
  d_tbl: (word * word, word option) Hashtbl.t;
  db_tbl: (word * word, WordSet.t option) Hashtbl.t; (*memo table for distinguish_blanks*)
  query_count: int ref;
}

let make (p: word list) (n: word list) =
  {
    pos = WordSet.of_list p;
    neg = WordSet.of_list n;
    d_tbl = Hashtbl.create 101;
    db_tbl = Hashtbl.create 101;
    query_count = ref 0;
  }

let conjecture (t: t) (c: Dfa.t) =
  match WordSet.filter (Dfa.accept c) (t.neg) |> WordSet.min_elt_opt with
  | Some w -> Some w
  | None -> WordSet.filter (fun w -> not (Dfa.accept c w)) (t.pos) |> WordSet.min_elt_opt

let query (t: t) (w: word) =
  let () = t.query_count := !(t.query_count) + 1 in
  if WordSet.mem w t.pos then
    Some true
  else if WordSet.mem w t.neg then
    Some false
  else
    None

let number_queries (t:t) : int = !(t.query_count)

let resid_by (w: word) (s: WordSet.t) = WordSet.filter_map (Word.resid w) s

(* [compute_distinguish t s1 s2] returns (Some suffix) if s1@suffix is in pos
   and s2@suffix is neg or s1@suffix is in neg and s2@suffix is in pos; and
   None if no such suffix exists. *)
let compute_distinguish (t: t) (s1: word) (s2: word) : word option =
  let pn = WordSet.inter (resid_by s1 t.pos) (resid_by s2 t.neg) in
  let np = WordSet.inter (resid_by s1 t.neg) (resid_by s2 t.pos) in
  match WordSet.union pn np with
  | e when WordSet.is_empty e -> None
  | s -> Some (WordSet.min_elt s)

(* Memoized distinguish query *)
let distinguish_concrete (t: t) (s1: word) (s2: word) : word option =
  let s_sm, s_lg = if s1 < s2 then s1, s2 else s2, s1 in
  match Hashtbl.find_opt t.d_tbl (s_sm, s_lg) with
  | None -> let r = compute_distinguish t s_sm s_lg in
            let () = Hashtbl.add t.d_tbl (s_sm, s_lg) r in
            r
  | Some r -> r

let compute_dist_b t s1 s2 : WordSet.t = 
  let pos_s1_suf, neg_s1_suf, pos_s2_suf, neg_s2_suf = (resid_by s1 t.pos),
    (resid_by s1 t.neg), (resid_by s2 t.pos), (resid_by s2 t.neg) in
  let p_i = WordSet.inter pos_s1_suf pos_s2_suf in
  let n_i = WordSet.inter neg_s1_suf neg_s2_suf in 
  let p_u = WordSet.union pos_s1_suf pos_s2_suf in
  let n_u = WordSet.union neg_s1_suf neg_s2_suf in 
  WordSet.(union (diff p_u p_i) (diff n_u n_i))

let distinguish (t:t) (s1: word) (s2: word) (cols: WordSet.t) = 
  match distinguish_concrete t s1 s2 with 
  | Some r when not (WordSet.mem r cols) -> Some r
  | _ ->
    let s_sm, s_lg = if s1 < s2 then s1, s2 else s2, s1 in
    match Hashtbl.find_opt t.db_tbl (s_sm, s_lg) with
    | None -> let sufs = compute_dist_b t s_sm s_lg in
              let sufs' = WordSet.diff sufs cols in
              let sufs_opt = if sufs' = WordSet.empty then None else Some sufs' in
              let () = Hashtbl.add t.db_tbl (s_sm, s_lg) sufs_opt in
              sufs' |> WordSet.min_elt_opt
    | Some Some sufs -> WordSet.(diff sufs cols |> min_elt_opt)
    | _ -> None
