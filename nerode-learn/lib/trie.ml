open Nerode

type symbol = Alphabet.symbol
type word = Word.t

module SymOrdered = struct
  type t = Alphabet.symbol
  let compare a0 a1 = Alphabet.compare a0 a1
end
module SymMap = Map.Make(SymOrdered)

type 'a t = Null | Leaf of 'a | Node of 'a * 'a t SymMap.t

let empty = Null
let eps (v: 'a) = Leaf v

let rec print (t: 'a t) (a: Alphabet.t) (to_str: 'a -> string) =
  let () = match t with
  | Null -> Printf.printf "Null"
  | Leaf v -> Printf.printf "[%s]" (to_str v)
  | Node (v, m) -> Printf.printf "[%s](" (to_str v);
                   SymMap.iter (fun k v -> 
                     Printf.printf "%s->" (Alphabet.sym_to_string a k);
                     print v a to_str;
                     Printf.printf " ") m;
                   Printf.printf ")" in
  Printf.printf "%!"


let rec find (t: 'a t) (w: word) =
  match t with
  | Null -> failwith "Not found!"
  | Leaf v -> if w = [] then v else failwith "Not found!"
  | Node (v, m) -> match w with
                   | [] -> v
                   | x::tail -> let t' = SymMap.find x m in
                                find t' tail

let find_opt (f: word -> 'a -> bool) (t: 'a t) : (word*'a) option =
  let rec fw f w t =
    match t with
    | Null -> None
    | Leaf v -> if f w v then Some (w,v) else None
    | Node (v,m) -> if f w v then Some (w,v) else
                    match List.find_opt (fun (x, t') ->
                              match fw f (w@[x]) t' with
                              | None -> false
                              | Some _ -> true) (SymMap.bindings m) with
                    | None -> None
                    | Some (x, t') -> fw f (w@[x]) t' in
  fw f [] t

let rec map (update: word -> 'a -> 'b) (t: 'a t) : 'b t =
  match t with
  | Null -> Null
  | Leaf v -> Leaf (update [] v)
  | Node (v, m) -> let v' = (update [] v) in
                   let m' = SymMap.filter_map (fun k v ->
                     let update_k s = update ([k]@s) in
                     Some (map update_k v)) m in
                   Node (v', m')

let eps_extend (get: word -> 'a) (alpha: Alphabet.t) =
  let add_x m x =
    SymMap.add x (Leaf (get [x])) m in
  let ext = Alphabet.fold add_x SymMap.empty alpha in
  Node (get [], ext)

let rec of_word (w: word) (get: word -> 'a) : 'a t =
  match w with
  | [] -> Leaf (get [])
  | x::tail -> let getx s = get (x::s) in
               Node (get [], SymMap.singleton x (of_word tail getx))

let rec of_word_extend (w: word) (alpha: Alphabet.t) (get: word -> 'a) : 'a t =
  let not_x x =
    List.filter (fun a -> Alphabet.compare a x <> 0) (Alphabet.symbols alpha) in
  match w with
  | [] -> eps_extend get alpha
  | x::tail -> let getx s = get (x::s) in
               let m = SymMap.singleton x (of_word_extend tail alpha getx) in
               let m' = List.fold_left (fun acc a -> 
                 SymMap.add a (Leaf (get [a])) acc) m (not_x x) in
               Node (get [], m')

let rec add (get: word -> 'a) (t: 'a t) (s: word) : 'a t =
  match t with
  | Null
  | Leaf _ -> of_word s get (* Does this unecessarily call get? *)
  | Node (v, m) -> match s with
                   | [] -> t
                   | x::tail -> let getx s = get (x::s) in
                                match SymMap.find_opt x m with
                                | None -> let t' = of_word tail getx in
                                          let m' = SymMap.add x t' m in
                                          Node (v, m')
                                | Some t' -> let t'' = add getx t' tail in
                                             let m' = SymMap.add x t'' m in
                                             Node (v, m')

let rec add_extend (get: word -> 'a) (alpha: Alphabet.t) (t: 'a t) (w: word) : 'a t =
  match t with
  | Null
  | Leaf _ -> of_word_extend w alpha get
  | Node (v,m) -> match w with
                  | [] -> t
                  | x::tail -> let getx s = get (x::s) in
                               match SymMap.find_opt x m with
                               | None -> let t' = of_word_extend tail alpha getx in
                                         let m' = SymMap.add x t' m in
                                         Node (v, m')
                               | Some t' -> let t'' = add_extend getx alpha t' tail in
                                            let m' = SymMap.add x t'' m in
                                            Node (v, m')

let of_wordlist (get: word -> 'a) (s: word list) = List.fold_left (add get) Null s

let of_wordlist_extend (get: word -> 'a) (alpha: Alphabet.t) (s: word list) =
  List.fold_left (add_extend get alpha) Null s

let rec keys (t: 'a t): word list =
  match t with
  | Null -> []
  | Leaf _ -> [[]]
  | Node (_,n) -> []::(SymMap.fold (fun k d acc ->
                                    (List.map (fun e -> [k]@e) (keys d))@acc) n [])

let rec node_keys (t: 'a t): word list =
  match t with
  | Null
  | Leaf _ -> []
  | Node (_,n) -> []::(SymMap.fold (fun k d acc ->
                                    (List.map (fun e -> [k]@e) (node_keys d))@acc) n [])

let rec leaf_keys (t: 'a t): word list =
  match t with
  | Null -> []
  | Leaf _ -> [[]]
  | Node (_,n) -> (SymMap.fold (fun k d acc ->
                              (List.map (fun e -> [k]@e) (leaf_keys d))@acc) n [])

let rec bindings (t: 'a t): (word * 'a) list =
  match t with
  | Null -> []
  | Leaf v -> [([],v)]
  | Node (v,n) -> ([],v)::(SymMap.fold (fun k d acc ->
                                    (List.map (fun (w, e) -> ([k]@w, e)) (bindings d))@acc) n [])

let rec node_bindings (t: 'a t): (word * 'a) list =
  match t with
  | Null
  | Leaf _ -> []
  | Node (v,n) -> ([],v)::(SymMap.fold (fun k d acc ->
                                    (List.map (fun (w, e) -> ([k]@w, e)) (node_bindings d))@acc) n [])

let rec leaf_bindings (t: 'a t): (word * 'a) list =
  match t with
  | Null -> []
  | Leaf v -> [([],v)]
  | Node (_,n) -> (SymMap.fold (fun k d acc ->
                              (List.map (fun (w, e) -> ([k]@w, e)) (leaf_bindings d))@acc) n [])

let suffixes (s: word list) =
  List.map List.rev s |> of_wordlist (fun _ -> ())
                      |> keys
                      |> List.map List.rev

let prefixes (s: word list) = of_wordlist (fun _ -> ()) s |> keys

(* TODO: Update these to fold/iter keys and values instead of just keys *)
let fold f a t = List.fold_left f a (keys t)
let iter f t = List.iter f (keys t)

