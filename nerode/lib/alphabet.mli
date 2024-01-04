(** Represents a finite alphabet of symbols. *)

(** A single symbol in an alphabet. *)
type symbol

(** An alphabet: a finite collection of symbols. *)
type t

(** [intalph k] returns an alphabet containing the symbols \{0, 1, ..., k-1\},
    with string representations "0", "1", ... *)
val intalph : int -> t

(** [size alpha] returns the size of [alpha] *)
val size : t -> int

(** [of_string_array a] constructs an alphabet with symbols corresponding to the
    strings in [a]. *)
val of_string_array : string array -> t

(** [compare x y] returns a number greater than 0 if x is greater than y, 0 if
  x = y,  and less than 0 if x is less than y. *)
val compare : symbol -> symbol -> int

(** [symbols  alpha] returns a list of the symbols in this alphabet. *)
val symbols : t -> symbol list

(** [iter f alpha] performs [f x] for each symbol [x] in [alpha]. *)
val iter : (symbol->unit) -> t -> unit

(** [fold f init alpha] folds the function [f] over [alpha], i.e.,
    returning [f ( f init x1 ) x2 ...] for all the symbols [x1, x2, ...] in [alpha]. *)
val fold :  ('a->symbol->'a) -> 'a -> t -> 'a

(** [map f alpha] returns a list [[f x1, f x2, ...]] for the symbols
    [x1, x2, ...] in [alpha]. *)
val map : (symbol->'a) -> t -> 'a list


(** Serialize to S-Expression. *)
val sym_of_sexp : Core.Sexp.t -> symbol

(** Deserialize from S-Expression. *)
val sexp_of_sym : symbol -> Core.Sexp.t

(** Serialize symbol to JSON. *)
val sym_of_json : Yojson.Basic.t -> symbol

(** Deserialize symbol from JSON. *)
val sym_to_json : symbol -> Yojson.Basic.t

(** Serialize alphabet to JSON. *)
val of_json : Yojson.Basic.t -> t

(** Deserialize alphabet from JSON. *)
val to_json : t -> Yojson.Basic.t

(** Convert a symbol to its string representation. *)
val sym_to_string : t -> symbol -> string

(** Convert an alphabet to its string representation. *)
val to_string : t -> string

(** Convert (injectively) a symbol to an integer. *)
val sym_to_int : symbol -> int

(** Convert an integer to a symbol. It is an error to convert an integer greater
    than or equal to the size of the alphabet. *)
val sym_of_int : int -> symbol
