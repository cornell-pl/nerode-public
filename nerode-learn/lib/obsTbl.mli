(** An Angluin/Gold-style observation table for a learner to use to store
    information from the teacher. *)

open Nerode

module WordSet : Set.S with type elt = Word.t
module WordMap : Map.S with type key = Word.t and type +'a t = 'a Map.Make(Word).t
module RowLabels : Set.S with type elt = Word.t and type t = WordSet.t
module ColLabels : Set.S with type elt = Word.t and type t = WordSet.t

type entry =
  | True
  | False
  | Blank
val compare : entry -> entry -> int

type t

(** [upper_rows tbl] returns the upper rows (specifically the row labels, not the entries) 
in table [tbl]*)
val upper_row_labels : t -> RowLabels.t

(** [max_upper_row_label tbl] returns the upper row label of [tbl] which is
lexicographically greatest. *)
val max_upper_row_label : t -> Word.t

(** [lower_rows] returns the unique set containing all possible
one-letter extensions of the words/row labels in [upper_rows], minus those already
in [upper_rows]*)
val lower_row_labels : t -> RowLabels.t

(** [cols tbl] returns the columns (specifically the column labels, not the entries) 
in table [tbl]*)
val col_labels : t -> ColLabels.t

(** [get_blanks tbl] returns the set of words whose corresponding entries in
the table [tbl] are blanks*)
val get_blanks : t -> WordSet.t

(** [lookup_by_word_opt w t] returns [Some entry] corresponding to word [w] in 
table [t] if there is an entry for [w], or [None] if there is no entry for 
[w] in [t]*)
val lookup_by_word_opt : Word.t -> t -> entry option

(** [lookup_by_word_opt w t] returns [Some entry] in row [row] and column [col]
in table [t] if there is an entry in row [row] and column [col], or [None] if 
there is no entry for row [row] and column [col] in [t]*)
val lookup_by_rowcol_opt : Word.t -> Word.t -> t -> entry option

(** [add_row em get_entry w tbl] takes a label of a row [w] and adds the 
corresponding entries for the row in each of the columns 
in the table [tbl], returning the table with the added entries, 
plus an updated entry map with the bindings of [em] and those learned with 
membership queries when expanding the table. The new table
also includes the one-letter extensions of [w] to the bottom rows 
(if not already in the upper rows) as well as the corresponding entries for 
those rows if they are not already in the table. It takes in a
function [get_entry] that returns a corresponding entry for a given word*)
val add_row : (Word.t -> entry) -> entry WordMap.t -> Word.t -> t -> t * entry WordMap.t

(** [add_col em get_entry w tbl] returns 1) a table containing everything from [tbl]
plus the column with label [w] and the corresponding entries for the column
in each of the (upper and lower) rows in [tbl] and 2) updated entry map with 
the bindings of [em] and the new mappings learned with membership queries when 
expanding the table. It takes in a function [get_entry] that returns a 
corresponding entry for a given word*)
val add_col : (Word.t -> entry) -> entry WordMap.t -> Word.t -> t -> t * entry WordMap.t

(** [add_cols get_entry col_lst tbl] returns a table containing everything 
from [tbl] plus the columns with label l for each l in [col_lst] as well as 
the corresponding entries for each column in each of the (upper and lower)
rows in [tbl]. It takes in a function [get_entry] that returns a 
corresponding entry for a given word*)
val add_cols : (Word.t -> entry) -> entry WordMap.t -> ColLabels.t -> t -> t * entry WordMap.t

(** [row_entries row table] returns a list of the entries in the row with 
label [row] in [table]. If the row is not in the table, an error is raised*)
val row_entries : Word.t -> t -> entry list 

(** [up_rows_labels_entries table] returns a list of unique pairs, where each 
pair is a row label in upper row of [table] and a list of the entries in that 
row in the same order as columns in the table. *)
val up_rows_labels_entries : t -> (Word.t * entry list) list

(** [low_rows_labels_entries table] returns a list of unique pairs, where each 
pair is a row label in lower rows of [table] and a list of the entries in that
row in the same order as columns in the table.*)
val low_rows_labels_entries : t -> (Word.t * entry list) list

(** [closed table] returns [None] if the [table] is closed, otherwise returns 
[Some lower_row_label] whose row should be added to the upper rows 
(the lower row is not similar to any of the upper rows)*)
val closed : t -> Word.t option

(** [similarity table] gives a _heuristic_ measure of the similarity of its rows.
Lower numbers mean _less_ similar, with 0 indicating that all distinctions
are supported by evidence strings*)
val similarity : t -> int

(** [filled_blanks table map] returns a table with the contents of [table], 
except that the blanks in the table 
Precondition: [map] must contain a binding for a word to a non-blank entry 
for every word that has a corresponding blank in [table], and no other keys
can exist in [map]*)
val fill_blanks : t -> entry Map.Make(Word).t -> t

(** [init_epsilon alphab get_entry] initializes a table over alphabet [alpha] with a
    function [get_entry],
mapping words to their corresponding entry, returning 1) a 1x1 table with row ε
and column ε, thus having one entry corresponding the string ε in the upper part*)
val init_epsilon : Alphabet.t -> (Word.t -> entry) -> t * (entry WordMap.t)

(** [print_table t] prints the table [t]:
The column labels are in the horizontal axis in lexicographic order.
The upper row labels are in the vertical axis first, in lexicogrphic order.
The lower row labels are in the vertical axis next, separated from the upper
row by "---", in lexicogrphic order.
The entry in row with label r and column with label c in the table is the entry 
corresponding to the word r^c, or rc*)
val print_table : t -> unit

(** Precondition: the table cannot contain any blank entries*)
val to_dfa : t -> Dfa.t

val equivalent_pairs : t -> (Word.t * Word.t) list

