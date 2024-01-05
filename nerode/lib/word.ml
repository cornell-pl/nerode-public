(** A word [Word.t] is implemented as an symbol list, with each symbol 
representing a letter in the word, with the same order in the list as in the word.*)
type t = Alphabet.symbol list
let compare = List.compare Alphabet.compare
let epsilon = []
let suffixes w = 
  List.fold_right (fun l acc -> (l::List.hd acc)::acc) w [epsilon]
let prefixes w = suffixes (List.rev w) |> List.map List.rev |> List.rev
let append_letter w (l : Alphabet.symbol) : t = w @ [l]
let concat w1 w2 = w1 @ w2  
let of_intlist lst = List.map Alphabet.sym_of_int lst
let to_intlist (w: t) = List.map Alphabet.sym_to_int w
let to_string (alpha: Alphabet.t) (w: t) = if w = epsilon then "ε" else List.fold_left 
  (fun acc l -> acc^(l |> Alphabet.sym_to_string alpha)) "" w
let to_symlist = Fun.id
let of_symlist = Fun.id

let rec prefix_of (s1: t) (s2: t) : bool =
  match s1, s2 with
  | [], _ -> true
  | a::s1_tail, b::s2_tail when a = b -> prefix_of s1_tail s2_tail
  | _, _ -> false

let rec resid (pre: t) (w: t) : t option =
  match pre, w with
  | [], _ -> Some w
  | a::s1_tail, b::s2_tail when a = b -> resid s1_tail s2_tail
  | _, _ -> None

let words_of_strings (alpha: Alphabet.t) s =
  let rec idx s i =
    if String.equal (Alphabet.sym_of_int i |> Alphabet.sym_to_string alpha) s then
      i
    else
      idx s (i+1) in
  let index s = idx s 0 in
  let rec consume rem acc =
    match rem with
    | [] -> acc
    | hd::tl ->
      if hd = "X" then (* X is wildcard: add each symbol to every word in acc *)
        consume tl (List.fold_left (fun a2 w -> 
              List.fold_left (fun a3 x -> (x::w)::a3) a2 (Alphabet.symbols alpha)) [] acc)
      else
        consume tl (List.map (fun w -> (index hd |> Alphabet.sym_of_int)::w) acc)
  in
  consume (List.rev s) [[]]
