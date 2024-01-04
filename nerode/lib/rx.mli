(** Representation of a Regular expression. *)

open Alphabet

type t =
  | Empty
  | Epsilon
  | Char of symbol
  | Seq of t list
  | Union of t list
  | Star of t
  | QMark of t
  | Intersect of t list
  | Neg of t

(** Provides a comparison using the standard interface to [compare] *)
val compare : t -> t -> int

(** [equiv r] decides if the two regexs are *syntactically* equivalent *)
val equiv : t -> t -> bool

(** Construct a regular expression which is the concatenation of a list of
    regular expressions. *)
val seq : t list -> t

(** Construct a regular expression which is the concatentation of the two
    given regular expressions. *)
val seq_pair : t -> t -> t

(** Construct a regular expression which is the union of a list of
    regular expressions. *)
val union : t list -> t

(** Construct a regular expression which is the union of the two
    given regular expressions. *)
val union_pair : t -> t -> t

(** Construct a regular expression which is the intersection of a list of
    regular expressions. *)
val intersect : t list -> t

(** Construct a regular expression which is the intersection of the two
    given regular expressions. *)
val intersect_pair : t -> t -> t

(** Construct a regular expression which is the Kleene star of the given
    regular expression. *)
val star : t -> t

(** Construct a regular expression which is the union of the given regular
    expression and [Empty]. *)
val qmark : t -> t

(** Construct a regular expression which is the complement of the given regular
    expression. *)
val neg : t -> t

(** Construct a regular expression which is the difference (as sets) of first
    regular expression and the second one. *)
val difference : t -> t -> t

(** Return the string representation of a regular expression. *)
val to_string : Alphabet.t -> t -> string

(** Given a word [w], construct a regular expression for [{w}]. *)
val of_word : Word.t -> t

(** Syntactic Brzozowski nullable predicate *)
val e : t -> bool

(** Syntatic Brzozowski derivative *)
val d : symbol -> t -> t
