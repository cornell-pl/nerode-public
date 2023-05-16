open Stdlib
open Nerode

(** A word [Word.t] is implemented as an symbol list, with each symbol 
representing a letter in the word, with the same order in the list as in the word.*)
type t = Alphabet.symbol list
let compare = List.compare Alphabet.compare
let epsilon = []
let suffixes w = 
  List.fold_right (fun l acc -> (l::List.hd acc)::acc) w [epsilon]
let append_letter w (l : Alphabet.symbol) : t = w @ [l]
let concat w1 w2 = w1 @ w2  
let of_intlist lst = List.map Alphabet.sym_of_int lst
let to_intlist (w: t) = List.map Alphabet.sym_to_int w
let to_string (w: t) = if w = epsilon then "ε" else List.fold_left 
  (fun acc l -> acc^(l |> Alphabet.sym_to_int |> string_of_int)) "" w
let to_symlist = Fun.id
let of_symlist = Fun.id

