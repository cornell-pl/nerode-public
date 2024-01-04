open Stdlib
open Nerode

module WordSet = Set.Make(Word)
module WordMap = Map.Make(Word)
module RowLabels = WordSet
module ColLabels = WordSet

type entry =
  | True
  | False
  | Blank

let compare e1 e2 =
  match e1, e2 with
  | True, True
  | False, False
  | Blank, _
  | _, Blank -> 0
  | True, _ -> 1
  | _, True -> -1

let string_of_entry = function
  | True -> " + "
  | False -> " - "
  | Blank -> "[ ]"

let similar b1 b2 = compare b1 b2 = 0

module RowEntries = struct include List type t = entry list end

let identical = RowEntries.equal ( = )

module TblMap = WordMap

(*AF: the table is a record, where [alpha] is the alphabet, [upper_rows]
contains the row labels of the table, [cols] contains the column labels of the
table, and [tbl] is a map matching words to entries of type entry, where the
word is a prefix-suffix combination of a row and column label in the table, and
the corresponding entry describes the entry for that word in the table.
[lower_rows] are all the possible one-letter extensions of the words in
[upper_rows], not including those extensions that are already in [upper_rows].
The entries along its rows are also stored in [tbl]. 
RI: For all S in [upper_rows] and [lower_rows] and for all E in [cols], 
there is a corresponding word with an entry assigned to it in [tbl] for the 
prefix-suffix concatenation/word SE; conversely, for every word W with an 
entry in [tbl], there exists an S in [upper_rows] and E in [cols] such that W = SE, 
aka the concatenation of S and E. 
Additionally, [lower_rows] must be the unique set containing all possible
one-letter extensions of the words/row labels in [upper_rows], minus those already
in [upper_rows].*)
type t = {
  alpha: Alphabet.t;
  upper_row_labels : RowLabels.t;
  lower_row_labels : RowLabels.t;
  col_labels : ColLabels.t; (*Columns are maintained in lexicographical order currently*)
  tbl : entry TblMap.t} 

let upper_row_labels tbl = tbl.upper_row_labels
let max_upper_row_label tbl = RowLabels.max_elt tbl.upper_row_labels
let col_labels tbl = tbl.col_labels
let lower_row_labels tbl = tbl.lower_row_labels
let lookup_by_word_opt w table = TblMap.find_opt w table.tbl
let lookup_by_rowcol_opt rowlbl collbl table = 
  lookup_by_word_opt (Word.concat rowlbl collbl) table

let get_blanks table : WordSet.t = 
  TblMap.(filter (fun _ e -> e = Blank) table.tbl |> bindings) 
  |> List.split |> fst |> WordSet.of_list

(* helper for [add_row] and [init_epsilon]*)
let rows_from_adding_letter alpha row = 
  (*accumulating possible words from adding a letter (either 0 or 1)
  to a given word [row]*)
  List.fold_left (fun acc a -> 
    let new_w = Word.append_letter row a in
    RowLabels.add (new_w) acc) RowLabels.empty (Alphabet.symbols alpha)

(*helper for [add_row] and [add_col]*)
let add_entries get_entry e_map pre_and_suf (table : t) = 
  let tbl', e_map' = List.fold_left (fun (tblacc, em_acc) w -> 
    match TblMap.find_opt w em_acc with
      | Some entry -> TblMap.add w entry tblacc, em_acc
      | None -> let w_ent = get_entry w in
                TblMap.add w w_ent tblacc, TblMap.add w w_ent em_acc
    ) (table.tbl, e_map) pre_and_suf in
  {table with tbl = tbl'}, e_map'

let add_row get_entry e_map (w: Word.t) (table: t) : t * entry TblMap.t = 
  let col_lst = ColLabels.elements table.col_labels in
  let u_r_ls = table.upper_row_labels in
  let new_lowers = RowLabels.(rows_from_adding_letter table.alpha w (*new possible additions to lower_labels*)
    |> filter (fun sa -> not (mem sa u_r_ls))) in(*filtering out words already in upper_labels*)
  let new_l_r_l = (*new lower_rows_labels, add new lowers to old and remove [w] from it*)
    RowLabels.(union table.lower_row_labels new_lowers |> remove w) in
  let tbl_newrows = {table with 
    upper_row_labels = RowLabels.add w u_r_ls; 
    lower_row_labels = new_l_r_l} in
  let pre_and_suf = (*list of all possible new rowlabel-column label combinations to add to table*)
    let new_rs = RowLabels.add w new_lowers in
    RowLabels.fold (fun row lstacc -> 
      lstacc @ (List.map (fun col -> Word.concat row col) col_lst)) new_rs [] 
  in
  add_entries get_entry e_map pre_and_suf tbl_newrows

let add_col get_entry e_map (w: Word.t) (table: t) : t * entry TblMap.t = 
  let all_rows_lst = 
    RowLabels.(union table.upper_row_labels table.lower_row_labels |> elements) in
  let pre_and_suf = 
    List.map (fun row -> Word.concat row w) all_rows_lst in
  let tbl' = {table with col_labels = ColLabels.add w table.col_labels} in
  add_entries get_entry e_map pre_and_suf tbl'

let add_cols get_entry e_map (new_cols: ColLabels.t) table = 
  ColLabels.fold (fun w (tblacc, em_acc) -> add_col get_entry em_acc w tblacc) new_cols (table, e_map)

(** Initializes the 1x1 table with S and E as both just epsilon, 
    so the only entry is for the empty string. *)
let init_epsilon alpha get_entry : t * entry TblMap.t = 
  let s : RowLabels.t = RowLabels.(empty |> add Word.epsilon) in
  let sa : RowLabels.t = rows_from_adding_letter alpha Word.epsilon in
  let e : ColLabels.t = ColLabels.(empty |> add Word.epsilon) in
  let tbl : entry TblMap.t = RowLabels.(fold (fun w tblacc ->
    TblMap.add w (get_entry w) tblacc) (union s sa) TblMap.empty) in
  { alpha = alpha;
  upper_row_labels = s; 
  lower_row_labels = sa; 
  col_labels = e; 
  tbl = tbl}, tbl

(** [row_entries row table] returns a list of the entries in the row with 
    label [row] in [table]. If the row is not in the table, an error is raised. *)
let row_entries (row: Word.t) table : RowEntries.t =
  table.col_labels |> ColLabels.elements 
    |> List.map (fun col -> lookup_by_rowcol_opt row col table |> Option.get)

let rows_labels_entries rowlbls table : (Word.t*RowEntries.t) list = 
  RowLabels.fold 
    (fun row acc -> (row, row_entries row table)::acc) rowlbls []

(** [up_row_labels_entries table] returns a list, whose elements are a pair
    containing a row's label and a list of its entries, for each upper row in
    [table]. *)
let up_rows_labels_entries table : (Word.t*RowEntries.t) list = 
  rows_labels_entries table.upper_row_labels table

let low_rows_labels_entries table = 
  rows_labels_entries table.lower_row_labels table

let rows_similar rlbl1 rlbl2 t = 
  RowEntries.equal similar (row_entries rlbl1 t) (row_entries rlbl2 t)


(*---------- Similarity heuristic functions: -------------------------------- *)
(* These functions give heuristics for estimating how likely S is to contain
    a redundant member. Low similarity is *not likely* to have a redundant row *)
let rows_similarity init rlbl1 rlbl2 t =
  let r1 = row_entries rlbl1 t in
  let r2 = row_entries rlbl2 t in
  if rows_similar rlbl1 rlbl2 t then
    RowEntries.fold_left2 (fun a e1 e2 ->
      a +  match e1,e2 with
            | True, True
            | False, False -> 1
            | Blank, _
            | _, Blank -> 0
            | _, _ -> failwith "Impossible for similar rows") init r1 r2
  else
    0

let allpairs_similarity (tbl:t) : int =
  RowLabels.fold (fun r1 a1 ->
    RowLabels.fold (fun r2 a2 ->
      if r1 < r2 then
        a2 + rows_similarity 0 r1 r2 tbl
      else
        a2
    ) tbl.upper_row_labels a1) tbl.upper_row_labels 0

let blanks_count (tbl: t) : int =
  RowLabels.fold (fun r a1 ->
    ColLabels.fold (fun e a2 ->
      a2 + match lookup_by_rowcol_opt r e tbl with
            | None -> failwith "table entry does not exist"
            | Some Blank -> 1
            | _ -> 0
    ) tbl.col_labels a1
  ) tbl.upper_row_labels 0

let blank_word_count (tbl: t) : int =
  let blank_words = RowLabels.fold (fun r a1 ->
    ColLabels.fold (fun e a2 ->
      WordSet.union a2 (match lookup_by_rowcol_opt r e tbl with
                        | None -> failwith "table entry does not exist"
                        | Some Blank -> WordSet.singleton (Word.concat r e)
                        | _ -> WordSet.empty)
    ) tbl.col_labels a1
  ) tbl.upper_row_labels WordSet.empty in
  WordSet.fold (fun w a -> a + match lookup_by_word_opt w tbl with
                                | None -> failwith "table entry does not exist"
                                | Some Blank -> 1
                                | _ -> 0 ) blank_words 0


let last_similarity (tbl:t) : int =
  let m = max_upper_row_label tbl in
  let sim s a =
    max (rows_similarity 0 m s tbl) a in
  RowLabels.fold sim tbl.upper_row_labels (-1)

let similarity = allpairs_similarity
(*--------------------------------------------------------------------------- *)

(*returns None if closed, otherwise Some lower row label which is not similar
with any of the upper rows*)
(*NOTE: not yet tested, consider renaming to be more clear?*)
let closed table: Word.t option =
  let not_in_s sa = table.upper_row_labels 
    |> RowLabels.for_all (fun s -> not (rows_similar s sa table)) in 
  table.lower_row_labels|> RowLabels.elements |> List.find_opt not_in_s

let fill_blanks table (map: entry TblMap.t): t = 
  let tbl' = TblMap.merge (fun _ mape tble -> 
      match mape, tble with
      | None, None -> None
      | _, None -> failwith "tried to fill word that doesn't exist in table"
      | Some Blank, _ -> failwith "tried to fill a blank with a blank"
      | Some entry, Some Blank -> Some entry
      | None, o -> o
      | _, _ -> failwith "tried to fill/replace a non-blank entry") 
    map table.tbl in
  {table with tbl = tbl'}

let word_to_string w table =
  Word.to_string table.alpha w

let print_row rlbl table = 
  word_to_string rlbl table |> Printf.printf "%s ";
  RowEntries.iter 
    (fun e -> string_of_entry e |> Printf.printf "%s ") 
    (row_entries rlbl table);
  Printf.printf "\n%!"

let print_table table = 
  ColLabels.iter 
    (fun clbl -> word_to_string clbl table |> Printf.printf " %s") table.col_labels;
  Printf.printf "\n%!";
  RowLabels.iter (fun w -> print_row w table) table.upper_row_labels;
  Printf.printf "---\n%!";
  RowLabels.iter (fun w -> print_row w table) table.lower_row_labels

(* [is_same_state s0 s1] Returns true if s0 and s1 are the same state.*)
let is_same_state (table,(w0:Word.t)) (_,(w1:Word.t)) = 
  RowEntries.compare compare (row_entries w0 table) (row_entries w1 table) = 0

(*[delta a q] Returns the derivative of q wrt a; that is, the state we should
transition to on a from q.*)
let delta (letter:Alphabet.symbol) (table,w) = 
  let new_row_e = row_entries (Word.append_letter w letter) table in
  let ur_ls_es = up_rows_labels_entries table in
  let (label, _) = 
    List.find (fun (_, es) -> List.compare compare new_row_e es = 0) ur_ls_es in 
  (table, label)

(*[is_accepting q] Returns true if q is an accepting state.*)
let is_accepting (table, w) = match lookup_by_word_opt w table with
  | Some True -> true
  | None -> failwith "couldn't find entry in table for word"
  | Some Blank -> failwith "unfilled blank in table"
  | _ -> false

let to_dfa (table: t) = 
  let start = (table, Word.epsilon) in 
  Dfa.mk_dfa {eq=is_same_state; d=delta; e=is_accepting} table.alpha start

let equivalent_pairs table =  
  let ur_ls_es = up_rows_labels_entries table in
  RowLabels.fold 
    (fun lrl acc -> 
      let new_row_e = row_entries lrl table in
      let (url, _) = List.find (fun (_, es) -> 
          List.compare compare new_row_e es = 0) ur_ls_es in 
      (url, lrl)::acc) 
    table.lower_row_labels []

