(** An alternate implementation of the observation table that allows exploration
    of muliple tables with different rows to refer into the one large central
    data structure. *)

open Stdlib
open Nerode
open ObsTbl

module ColLabels = struct include List type t = Word.t list end

module type SuperTableSig = sig
  type entry =
    | True
    | False
    | Blank
  val compare : entry -> entry -> int

  type t
  
  (** [upper_row_labels tbl] returns the upper rows (specifically the row labels, not the entries) 
  in table [tbl]*)
  val upper_row_labels : t -> RowLabels.t

  (** [max_upper_row_label tbl] returns the upper row label of [tbl] which is
  lexicographically greatest. *)
  val max_upper_row_label : t -> Word.t

  (** [lower_rows_labels] returns the unique set containing all possible
  one-letter extensions of the words/row labels in [upper_rows], minus those already
  in [upper_rows]*)
  val lower_row_labels : t -> RowLabels.t

  (** [cols tbl] returns the columns (specifically the column labels, not the entries) 
  in table [tbl]*)
  val col_labels : t -> ColLabels.t 

  (** [add_row get_entry w tbl] takes a label of a row [w] and adds the 
  corresponding entries for the row in each of the columns 
  in the table [tbl], returning the table with the added entries. The new table
  also includes the one-letter extensions of [w] to the bottom rows 
  (if not already in the upper rows) as well as the corresponding entries for 
  those rows if they are not already in the table. It takes in a
  function [get_entry] that returns a corresponding entry for a given word*)
  val add_row : (Word.t -> entry) -> Word.t -> t -> t

  (** [add_col get_entry w tbl] returns a table containing everything from [tbl]
  plus the column with label [w] and the corresponding entries for the column
  in each of the (upper and lower) rows in [tbl]. It takes in a 
  function [get_entry] that returns a corresponding entry for a given word *)
  val add_col : (Word.t -> entry) -> Word.t -> t -> t
  
  (** [add_cols get_entry col_lst tbl] returns a table containing everything 
  from [tbl] plus the columns with label l for each l in [col_lst] as well as 
  the corresponding entries for each column in each of the (upper and lower)
  rows in [tbl]. It takes in a function [get_entry] that returns a 
  corresponding entry for a given word*)
  val add_cols : (Word.t -> entry) -> ColLabels.t -> t -> t

  (** [up_row_entries row table] returns a list of the entries in the row with 
  label [row] in the upper rows of [table]. 
  If the row is not in the table, an error is raised*)
  val up_row_entries : Word.t -> t -> entry list 

  (** [low_row_entries row table] returns a list of the entries in the row with 
  label [row] in the lower rows of [table]. 
  If the row is not in the table, an error is raised*)
  val low_row_entries : Word.t -> t -> entry list 

  (**[ up_rows_labels_entries table] returns a list of unique pairs, where each 
  pair is a row label in upper row of [table] and a list of the entries in that 
  row in the same order as columns in the table. *)
  val up_rows_labels_entries : t -> (Word.t * entry list) list

  (** [low_rows_labels_entries table] returns a list of unique pairs, where each 
  pair is a row label in lower rows of [table] and a list of the entries in that
  row in the same order as columns in the table.*)
  val low_rows_labels_entries : t -> (Word.t * entry list) list

  (** [init_epsilon get_entry] initializes a table with a function [get_entry]
  mapping words to their corresponding entry, returning a 1x1 (upper) table with 
  row ε and column ε, (as well as the one-letter extensions of ε in the lower
  rows) thus having one entry corresponding the string ε (and one entry 
  corresponding to each of the labels in the lower rows)*)
  val init_epsilon : (Word.t -> entry) -> t
end

module SuperTable : SuperTableSig = struct
  
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
  
  (** The table is a record, where [upper_row_labels] contains the row labels
  of the table, [col_labels] contains the column labels of the table, 
  [upper_row_entries] and [lower_row_entries] are maps matching upper and lower
  row labels to the row entries in their row, respectively.
  The entries for each row denoted by label [i] are ordered such that 
  entry [j] is the entry corresponding to the concatenation of [i] and 
  column label [j] in [col_labels]
  Invariants: For all S in [upper_rows] and [lower_rows], there exists a key S in 
  [upper_row_entries] and [lower_row_entries] respectively, with a binding 
  rowentry RE, for which entry [j] is the entry corresponding to the 
  prefix-suffix concatenation of S and column label [j] in [col_labels].
  and for all E in [cols]. The maps [upper_row_entries] and [lower_row_entries]
  cannot contain any other bindings. 
  The first element of [col_labels] must be the empty word epsilon.
  [upper_row_labels], [lower_row_labels], and [col_labels] contain no duplicate
  elements.
  Additionally, [lower_row_labels] must be the unique set containing all possible
  one-letter extensions of the words/row labels in [upper_row_labels], 
  minus those already in [upper_row_labels].*)
  type t = {
    upper_row_labels : RowLabels.t;
    upper_row_entries : RowEntries.t WordMap.t;
    lower_row_labels : RowLabels.t;
    lower_row_entries : RowEntries.t WordMap.t;
    col_labels : ColLabels.t (*Implemented as a Word.t list*)
    } 
  
  let upper_row_labels tbl = tbl.upper_row_labels
  let max_upper_row_label tbl =
    RowLabels.max_elt tbl.upper_row_labels
  let col_labels tbl = tbl.col_labels
  let lower_row_labels tbl = tbl.lower_row_labels

  (*helper for [add_row] and [init_epsilon]*)
  let rows_from_adding_letter row = 
    (*accumulating list of possible words from adding a letter (either 0 or 1)
    to a given word [row]*)
    List.fold_left (fun acc a -> 
      let new_w = Word.append_letter row (Alphabet.sym_of_int a) in
      (new_w)::acc) [] [0;1]
  
  (*helper for [add_row] and [init_epsilon]*)
  let add_entries get_entry row r_es col_lst : RowEntries.t WordMap.t = 
    let new_elst = List.map (fun col -> 
      Word.concat row col |> get_entry) col_lst in
    WordMap.add row new_elst r_es
  
  (*helper for [add_row] and [init_epsilon]*)
  let add_upper_entries get_entry w table : RowEntries.t WordMap.t = 
    add_entries get_entry w table.upper_row_entries table.col_labels
  
  (*helper for [add_row] and [init_epsilon]*)
  let add_lower_entries get_entry new_lowers (table : t) = 
    let new_l_r_es = List.fold_left (fun lre_acc row ->
          add_entries get_entry row lre_acc table.col_labels) 
        table.lower_row_entries new_lowers in 
    {table with lower_row_entries = new_l_r_es}
        
  
  let add_row get_entry (w: Word.t) (table: t) : t = 
    if RowLabels.mem w table.upper_row_labels then table
    else 
      let u_r_ls = table.upper_row_labels in
      let l_r_es = table.lower_row_entries in
      let new_lowers = rows_from_adding_letter w (*new possible additions to lower_labels*)
       |> List.filter (fun sa -> not (RowLabels.mem sa u_r_ls)) in (*filtering out words already in upper_labels*)
      let new_l_r_l = (*new lower_rows_labels, add new lowers to old and remove [w] from it*)
        RowLabels.(union table.lower_row_labels (of_list new_lowers) 
          |> remove w) in
      let new_u_es, l_r_es' = if RowLabels.mem w table.lower_row_labels
        then WordMap.(table.upper_row_entries |> add w (find w l_r_es), 
          remove w l_r_es) (*move [w] and its entries from lower rows to upper*)
        else add_upper_entries get_entry w table, l_r_es (*In wlsmt this should never have to be called*) 
        in
      let tbl_newrows = {table with 
        upper_row_labels = RowLabels.add w u_r_ls; 
        upper_row_entries = new_u_es;
        lower_row_labels = new_l_r_l;
        lower_row_entries = l_r_es'
        } in
      add_lower_entries get_entry new_lowers tbl_newrows

  (*NOTE: still needs to be tested*)
  let add_col get_entry (w: Word.t) (table: t) : t = 
    let update_res r_es = WordMap.mapi (fun rlbl -> 
      function
      | [] -> failwith "Panic! Empty row"
      | epse::tl -> epse::(Word.concat rlbl w |> get_entry)::tl) r_es in
    let new_u_r_es = update_res table.upper_row_entries in
    let new_l_r_es = update_res table.lower_row_entries in
    {table with 
    col_labels = begin 
      match table.col_labels with
        | [] -> failwith "Panic! Column labels can never be empty"
        | eps::tl -> eps::w::tl end;
    upper_row_entries = new_u_r_es;
    lower_row_entries = new_l_r_es}
  
  (*NOTE: still needs testing*)
  let add_cols get_entry (new_cols: ColLabels.t) table = 
    ColLabels.fold_left (fun tblacc w -> add_col get_entry w tblacc) table new_cols

  (**Initializes the 1x1 table with S and E as both just epsilon, and SA-S as 
  0 and 1, so the only entry is for the empty string*)
  let init_epsilon get_entry : t = 
    let s : RowLabels.t = RowLabels.(empty |> add Word.epsilon) in
    let sa_lst = rows_from_adding_letter Word.epsilon in
    let sa : RowLabels.t = 
      sa_lst |> RowLabels.of_list in
    let e : ColLabels.t = [Word.epsilon] in
    let semi_tbl =
      { upper_row_labels = s;
      upper_row_entries = WordMap.empty;
      lower_row_labels = sa;
      lower_row_entries = WordMap.empty;
      col_labels = e } in
    let s_es : RowEntries.t WordMap.t = 
      add_upper_entries get_entry Word.epsilon semi_tbl in
    add_lower_entries get_entry sa_lst {semi_tbl with upper_row_entries = s_es}

  let init_epsilon2 get_entry : t = 
    let s : RowLabels.t = RowLabels.(empty |> add Word.epsilon) in
    let sa_lst = rows_from_adding_letter Word.epsilon in
    let sa : RowLabels.t = 
      sa_lst |> RowLabels.of_list in
    let e : ColLabels.t = [Word.epsilon] in
    let s_es : RowEntries.t WordMap.t = 
      WordMap.singleton Word.epsilon [get_entry Word.epsilon] in
    let sa_es = (*implemented based on {0, 1} alphabet assumption*)
      let zero = Word.of_intlist [0] in
      let one = Word.of_intlist [1] in
      WordMap.(singleton zero [get_entry zero] |> add one [get_entry one]) 
    in
    { upper_row_labels = s;
      upper_row_entries = s_es;
      lower_row_labels = sa;
      lower_row_entries = sa_es;
      col_labels = e }
    
  let up_row_entries (row: Word.t) table : RowEntries.t =
    WordMap.find row table.upper_row_entries

  let low_row_entries (row: Word.t) table : RowEntries.t =
    WordMap.find row table.lower_row_entries

  (**[up_row_labels_entries table] returns a list, whose elements are a pair
  containing a row's label and a list of its entries, for each upper row in [table]
  NOTE: could be made into set instead*)
  let up_rows_labels_entries table : (Word.t*RowEntries.t) list = 
    WordMap.bindings table.upper_row_entries

  let low_rows_labels_entries table = 
    WordMap.bindings table.lower_row_entries
end
