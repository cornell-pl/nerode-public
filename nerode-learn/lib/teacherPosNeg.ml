(** Teacher initialized with a finite list of examples which flips a coin
    to answer all other membership queries (i.e., instead of returning Blank). *)

open Nerode

type word = Word.t

type t = bool Trie.t

let initialize_random (seed: int option) =
  match seed with
  | None -> Random.self_init ()
  | Some s -> Random.init s

let make (p: word list) (n: word list) : t =
  let f w = List.mem w p in
  Trie.of_wordlist f (p@n)

let conjecture (t: t) (c: Dfa.t) =
  match Trie.find_opt (fun w b -> b <> (Dfa.accept c w)) t with
  | Some (w,_) -> Some w
  | None -> None
  
let query (t: t) (w: word) =
  match Trie.find_opt (fun w' _ -> w = w') t with
  | None -> Some (Random.int 2 = 0) (* Not in trie; flip a coin *)
  | Some (w', b) -> Some b

let distinguish _ _ _ = failwith "Unsupported"

let distinguish_concrete _ _ _ _ = failwith "Unsupported"
