open Core

type symbol = Alphabet.symbol
type word = Word.t

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

let rec compare (t1:t) (t2:t) =
  match t1,t2 with
  | Empty, Empty -> 0
  | Empty, _ -> -1
  | _, Empty -> 1
  | Epsilon, Epsilon -> 0
  | Epsilon, _ -> -1
  | _, Epsilon -> 1
  | Char s1, Char s2 -> Alphabet.compare s1 s2
  | Char _, _ -> -1
  | _, Char _ -> 1
  | Seq lst1, Seq lst2 -> List.compare compare lst1 lst2
  | Seq _, _ -> -1
  | _, Seq _ -> 1
  | Union t1, Union t2 -> List.compare compare t1 t2
  | Union _, _ -> -1
  | _, Union _ -> 1
  | Star s1, Star s2 -> compare s1 s2
  | Star _, _ -> -1
  | _, Star _ -> 1
  | QMark s1, QMark s2 -> compare s1 s2
  | QMark _, _ -> -1
  | _, QMark _ -> 1
  | Intersect s1, Intersect s2 -> List.compare compare s1 s2
  | Intersect _, _ -> -1
  | _, Intersect _ -> 1
  | Neg s1, Neg s2 -> compare s1 s2

(* Syntactic equivalence *)
and equiv (r1:t) (r2:t) = ((compare r1 r2) = 0)

(* --- Rx smart constructors --- *)

(* Sort and remove adjacent duplicates *)
let usort (lst: t list) : t list =
  List.sort ~compare:compare lst |>
  List.fold_left ~f:(fun r x ->
    match r with
    | [] -> [x]
    | p::rem -> if equiv x p then r else x::r
    ) ~init:[] |>
  List.rev

let union (lst:t list) : t =
  let flatten a x =
    match x with
    | Union u -> u @ a
    | Empty -> a
    | _ -> x::a in
  let nonempty = List.fold_left ~f:flatten ~init:[] lst in
  match nonempty with
  | [] -> Empty
  | [r] -> r
  | _ -> Union (usort nonempty)

let union_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Empty, _ -> r2
  | _, Empty -> r1
  | Star r, Epsilon
  | Epsilon, Star r -> Star r
  | Union t1, Union t2 -> union (t1 @ t2)
  | Union t1, _ -> if List.exists ~f:(fun x -> equiv x r2) t1 then r1 else union (r2::t1)
  | _, Union t2 -> if List.exists ~f:(fun x -> equiv x r1) t2 then r2 else union (r1::t2)
  | _, _ -> if equiv r1 r2 then r1 else union [r1;r2]

let seq (lst1:t list) =
  let flatten a x =
    match x with
    | Seq s -> a @ s
    | Epsilon -> a
    | _ -> a @ [x] in
  let lst = List.fold_left ~f:flatten ~init:[] lst1 in
  match lst with
  | [] -> Epsilon
  | [r] -> r
  | _  -> if List.exists ~f:(fun x -> equiv x Empty) lst then Empty else Seq lst

let seq_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Empty, _ -> Empty
  | _, Empty -> Empty
  | Epsilon, _ -> r2
  | _, Epsilon -> r1
  | Seq t1, Seq t2 -> Seq (t1 @ t2)
  | Seq t1, _ -> Seq (t1 @ [r2])
  | _, Seq t2 -> Seq (r1::t2)
  | _, _ -> Seq [r1; r2]

let star (r0:t) : t =
  match r0 with
  | Epsilon | Empty -> Epsilon
  | Star _ -> r0
  | QMark r -> Star r
  | _ -> Star r0

let qmark (r:t) : t =
  match r with
  | Star _
  | Epsilon
  | QMark _ -> r
  | Empty -> Epsilon
  | _ -> QMark r

let intersect (lst:t list) : t =
  let flatten a x =
    match x with
    | Intersect i -> i @ a
    | _ -> x::a in
  let flat = List.fold_left ~f:flatten ~init:[] lst in
  match flat with
  | [] -> failwith "Nullary intersection undefined"
  | [r] -> r
  | _ -> if List.exists ~f:(fun x -> equiv x Empty) flat then Empty
         else Intersect (usort flat)

let intersect_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Empty, _
  | _, Empty -> Empty
  | Star r, Epsilon
  | Epsilon, Star r -> Epsilon
  | Epsilon, QMark r
  | QMark r, Epsilon -> Epsilon
  | QMark s1, Star s2 -> r1
  | Star s1, QMark s2 -> r2
  | Epsilon, Char x
  | Char x, Epsilon -> Empty
  | Char x, Char y -> if Alphabet.compare x y = 0 then r1 else Empty
  | Intersect t1, Intersect t2 -> Intersect (t1 @ t2)
  | Intersect t1, _ -> if List.exists ~f:(fun x -> equiv x r2) t1 then r1 else intersect (r2::t1)
  | _, Intersect t2 -> if List.exists ~f:(fun x -> equiv x r1) t2 then r2 else intersect (r1::t2)
  | _, _ -> if equiv r1 r2 then r1 else intersect [r1;r2]

let neg (r:t) : t =
  match r with
  | Neg s -> s
  (* Include DeMorgan?
  | Union lst -> intersect (List.map ~f:neg lst)
  | Intersect lst -> union (List.map ~f:neg lst)
  *)
  | _ -> Neg r

let difference (r1:t) (r2:t) : t =
  intersect_pair r1 (neg r2)

(* --- Pretty print --- *)
let to_string (alpha: Alphabet.t) (rx: t) : string =
  let prec (r:t): int =
    match r with
    | Union _ -> 0
    | Intersect _ -> 1
    | Seq _ -> 2
    | _ -> 3 in

  let rec to_string_parent (parent_prec: int) (r: t) : string =
    let s = match r with
    | Empty  -> "{}"
    | Epsilon -> "e"
    | Char r0 -> Alphabet.sym_to_string alpha r0
    | Seq r0 -> String.concat ~sep:"" (List.map ~f:(to_string_parent (prec r)) r0)
    | Union r0 -> String.concat ~sep:"+" (List.map ~f:(to_string_parent (prec r)) r0)
    | Star r0 -> (to_string_parent (prec r) r0) ^ "*"
    | QMark r0 -> (to_string_parent (prec r) r0) ^ "?"
    | Intersect r0 -> String.concat ~sep:"&" (List.map ~f:(to_string_parent (prec r)) r0)
    | Neg r0 -> (to_string_parent (prec r) r0) ^ "^" in

    if (prec r) < parent_prec then "(" ^ s ^ ")" else s in

  to_string_parent 0 rx

(* --- Brzozowski derivatives --- *)

let rec e (r:t) : bool =
  match r with
  | Empty -> false
  | Epsilon -> true
  | Char _ -> false
  | Seq r1 -> List.fold_left (List.map ~f:e r1) ~init:true ~f:(&&)
  | Union r1 -> List.fold_left (List.map ~f:e r1) ~init:false ~f:(||)
  | Star _ -> true
  | QMark _ -> true
  | Intersect r0 -> List.fold_left (List.map ~f:e r0) ~init:true ~f:(&&)
  | Neg r0 -> not (e r0)


let rec d (c:symbol) (r0:t) : t =
  match r0 with
  | Empty -> r0
  | Epsilon -> Empty
  | Char x ->
     if Alphabet.compare c x = 0 then Epsilon else Empty
  | Seq (r0::tail) ->
     let r0c_r2 = seq_pair (d c r0) (seq tail) in
     if e r0 then
       union_pair r0c_r2 (d c (seq tail))
     else
       r0c_r2
  | Union r -> union (List.map ~f:(d c) r)
  | Star r -> seq_pair (d c r) r0
  | QMark r -> d c r
  | Intersect r -> intersect (List.map ~f:(d c) r)
  | Neg r -> neg (d c r)

  | _ -> failwith "d: improper rx\n%!"

let rec matches (r:t) (u:word) : bool =
  match u with
  | [] ->
     e r
  | c::v ->
     matches (d c r) v

let rec of_word w = match w with
  | [] ->
      Epsilon
  | c::w ->
      seq_pair (Char c) (of_word w)
