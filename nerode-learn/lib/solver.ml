(** Provides the interface to the Z3 SMT solver for use by the learner. *)

open Core
open Nerode

type word = Alphabet.word 

module WordSet = ObsTbl.WordSet
module RowLabels = ObsTbl.RowLabels
module ColLabels = ObsTbl.ColLabels
module Table = ObsTbl

let rec to_string (w:int list) : string =
  match w with
  | [] -> ""
  | x::tail -> (string_of_int x)^(to_string tail)

open Z3.Smtlib

let real_z3_time = ref 0.0
let real_z3_queries = ref 0

module WordKey = struct
  type t = word
  let hash = Hashtbl.hash
  let compare = Stdlib.compare
  let sexp_of_t = List.sexp_of_t Alphabet.sexp_of_sym
  let t_of_sexp = List.t_of_sexp Alphabet.sym_of_sexp
end
module WordHash = Hashtbl.Make(WordKey)
module WordMap = Map.Make(WordKey)
module CamlWordMap = ObsTbl.WordMap

(* Convert w to labels for :named z3 assertions *)
let word_to_name (w:word) : string =
  let rec of_w s = match s with
  | [] -> ""
  | [s] -> string_of_int s
  | s::t -> (string_of_int s) ^ (of_w t) in
  "s"^(Word.of_symlist w |> Word.to_intlist |> of_w) (* z3 names have to start with a letter *)

(* Convert z3 names to w *)
let word_of_name (s:string) : Word.t =
  let len = (String.length s) - 1 in
  let s' = String.sub s ~pos:1 ~len:len in (* Chop off the letter *)
  List.init len ~f:(fun i -> (int_of_char s'.[i]) - (int_of_char '0'))
    |> Word.of_intlist

let solver = make_solver "z3"
let produce_unsats () =
  set_option solver ":produce-unsat-cores" true;
  set_option solver ":sat.core.minimize" true;
  set_option solver ":smt.core.minimize" true

let get_unsats () =
  List.fold_left ~init:WordSet.empty ~f:(fun set name -> WordSet.add (word_of_name name) set) (get_unsat_core solver)

let print_stats () : unit =
  Printf.printf "Z3_Time:\t%f\n%!" !real_z3_time;
  Printf.printf "Z3_Queries:\t%d\n%!" !real_z3_queries

let bv_of_t (t: Table.t) =
  let () = real_z3_queries := !real_z3_queries + 1 in
  reset solver;

  let next_blank =
    let cell = ref 0 in
    (fun () -> incr cell; !cell) in

  let b_var n = Printf.sprintf "b_%d" n in
  let s_var n = Printf.sprintf "s_%d" n in
  let sa_var n = Printf.sprintf "sa_%d" n in

  let term_of_entry =
    let one = bv 1 1 in
    let zero = bv 0 1 in
    let entries = WordHash.create () in
    let gensym w =
      let n = next_blank () in
      let b = b_var n in
      let t = const b in
      declare_const solver (Id b) (BitVecSort 1);
      WordHash.add_exn entries ~key:w ~data:t;
      t in

    let () = WordSet.iter (fun w -> ignore (gensym (Word.to_symlist w))) (Table.get_blanks t) in
    (fun w e ->
      let open Table in
      match e with
      | True -> one
      | False -> zero
      | Blank ->
         begin match WordHash.find entries w with
         | Some t -> t
         | None -> gensym w
         end) in

  let constrain_row mk_row i acc (u,r) =
    let get_e t = t |> Table.col_labels |> ColLabels.elements 
      |> List.map ~f:Word.to_symlist in
    let row = mk_row i in
    declare_const solver (Id row) (BitVecSort (List.length (get_e t)));
    let zipped = List.zip_exn r (get_e t) in
    List.iteri zipped ~f:(fun j (entry,suf) ->
        let w = u @ suf in
        let term = term_of_entry w entry in
        let extract = extract j j (const row) in
        assert_ solver (equals term extract));
    WordMap.set acc ~key:u ~data:(const row) in
  
  let map_conv = List.map ~f:(fun (w,elst) -> (Word.to_symlist w, elst)) in
  let s_rows = List.foldi (map_conv (Table.up_rows_labels_entries t)) 
    ~init:WordMap.empty ~f:(constrain_row s_var) in
  let sa_rows = List.foldi (map_conv (Table.low_rows_labels_entries t)) 
    ~init:WordMap.empty ~f:(constrain_row sa_var) in
  (s_rows, sa_rows, term_of_entry)

let assert_disjoint s_rows sa_rows =
  let terms = WordMap.to_alist s_rows |> List.map ~f:snd in
  assert_ solver (App (Id "distinct", terms))

let assert_closed s_rows sa_rows =
  let s_terms = WordMap.to_alist s_rows |> List.map ~f:snd in
  let sa_terms = WordMap.to_alist sa_rows in
  List.iter sa_terms ~f:(fun (s_i, sa_term) ->
      List.fold_left s_terms ~init:(bool_to_term false) ~f:(fun acc s_row -> or_ acc (equals sa_term s_row)) |>
      if CliOpt.unsat_cores_on () then
        assert_named solver (word_to_name s_i)
      else
        assert_ solver)

let assert_consistent alpha s_rows sa_rows =
  let lookup s =
    match WordMap.find s_rows s with
    | None -> WordMap.find_exn sa_rows s
    | Some term -> term in

  let s = WordMap.to_alist s_rows |> List.map ~f:fst in
  List.iteri s ~f:(fun i si ->
    List.iteri (List.filteri s ~f:(fun j _ -> j < i)) ~f:(fun j sj ->
      List.iter alpha ~f:(fun x ->
        assert_ solver (implies (equals (lookup si) (lookup sj)) 
                                (equals (lookup (si@[x])) (lookup (sj@[x]))));
        )))

let min_rows s_rows sa_rows =
  let lookup s = WordMap.find_exn s_rows s in
  let s = WordMap.to_alist s_rows |> List.map ~f:fst in
  let si_uniq s i si =
    let rows = List.filteri ~f:(fun j _ -> j < i) s in
    List.fold_left rows ~init:(bool_to_term true) ~f:(fun a r -> and_ a (not_ (equals (lookup r) (lookup si)))) in

  let u = List.foldi s ~init:[] ~f:(fun i a si ->
    let uid = "u"^(to_string (Alphabet.w_to_ints si)) in
    declare_const solver (Id uid) int_sort;
    let ui = const uid in
    let ui_unique = si_uniq s i si in

    let a1 = (implies (not_ ui_unique) (equals ui (Int(0)))) in
    let a2 = (implies ui_unique (equals ui (Int(1)))) in
    assert_ solver a1;
    assert_ solver a2;
    a @ [ui]) in
  let sum_u = List.fold_left u ~init:(Int(0)) ~f:(fun a ui -> add a ui) in
  minimize solver sum_u

let fill_blanks_asns (t: Table.t) assrt_lst =
  let (s_rows,sa_rows,toe) = bv_of_t t in
  let () = List.iter ~f:(fun a -> a s_rows sa_rows) assrt_lst in
  let start = Core_unix.gettimeofday () in
  let answer = check_sat solver in
  let () = real_z3_time := !real_z3_time +. (Core_unix.gettimeofday () -. start) in

  let get_results () = 
        let results = Caml.Hashtbl.create 11 in
        List.iter (get_model solver) ~f:(fun (id, term) ->
        let var = Const (id) in
        let b = term |> term_to_sexp |> sexp_to_string in
        match b with
        | "(_ bv1 1)" -> Caml.Hashtbl.add results var true
        | "(_ bv0 1)" -> Caml.Hashtbl.add results var false
        | _ -> ());
        results in
  let add_lookup res w acc = 
    let open Table in
    let term = toe w Blank in
    let entry = if Caml.Hashtbl.find res term then True else False in
    CamlWordMap.add (Word.of_symlist w) entry acc in

  let blanks = Table.get_blanks t in
  let results_map res = 
    WordSet.fold (fun w -> add_lookup res (Word.to_symlist w)) 
    blanks CamlWordMap.empty in

  match answer with
  | Unsat
  | Unknown -> None
  | Sat -> begin
      let m = get_results () |> results_map in
      Some (Table.fill_blanks t m)
  end

let fill_blanks (t: Table.t) = fill_blanks_asns t [assert_disjoint; assert_closed]
let fill_blanks_min_rows (t: Table.t) alpha = fill_blanks_asns t [assert_closed;
                                                                  assert_consistent alpha;
                                                                  min_rows]
