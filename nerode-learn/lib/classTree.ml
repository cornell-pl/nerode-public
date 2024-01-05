open Nerode

module WordSet = Set.Make(Word)
module StateMap = Map.Make(struct
  type t = Dfa.state
  let compare = Dfa.compare_states
end)

(* Internal nodes (Node) have a left subtree (reject path), right subtree
   (accept path), and a distinguishing suffix.  Leaf nodes have (Leaf) have an
   accessor prefix.  *)
type t = Node of (Word.t * t * t) | Leaf of Word.t

let init (rej: Word.t) (acc: Word.t) (suf: Word.t) =
  Node (suf, Leaf rej, Leaf acc)

let print (ct: t) (alpha: Alphabet.t) : unit =
  let to_string = Word.to_string alpha in
  let rec printr (ct: t) =
    match ct with
    | Node (d, rej, acc) -> to_string d |> Printf.printf "%s? (";
                            printr acc;
                            Printf.printf "):(";
                            printr rej;
                            Printf.printf ")"
    | Leaf s ->  to_string s |> Printf.printf "%s" in
  printr ct;
  Printf.printf "\n%!"

let rec sift (ct: t) (oracle: Word.t -> bool) (w: Word.t) =
  match ct with
  | Node (d, rej, acc) -> let child = if oracle (Word.concat w d) then acc else rej in
                          sift child oracle w
  | Leaf (s) -> s

(** [distinguish ct pre1 pre2] computes the distinguishing suffix for pre1 and
    pre2 by finding their lowest common ancestor. We do this by sifting both until
    we make different decisions. *)
let rec distinguish (ct: t) oracle (pre1: Word.t) (pre2: Word.t) =
  match ct with
  | Node (d, rej, acc) -> begin
                          match oracle (Word.concat pre1 d), oracle (Word.concat pre2 d) with
                          | false, false -> distinguish rej oracle pre1 pre2
                          | true, true -> distinguish acc oracle pre1 pre2
                          | _, _ -> d
                          end
  | Leaf (s) -> failwith "Leaf node cannot be a distinguishing suffix"

(** [hypothesis ct oracle alpha] computes the hypothesis DFA for [ct], over
    alphabet [alpha], using [oracle] for sifts. *)
let hypothesis (ct: t) (oracle: Word.t -> bool) (alpha: Alphabet.t)  =
  let eq w1 w2 = Word.compare w1 w2 = 0 in
  let d a w = sift ct oracle (Word.append_letter w a) in
  let e w = oracle w in
  Dfa.mk_dfa { eq; d; e } alpha Word.epsilon

let rec accessors (ct: t) : WordSet.t =
  match ct with
  | Node (d, rej, acc) -> WordSet.union (accessors rej) (accessors acc)
  | Leaf s ->  WordSet.singleton s

let rec distinguishers (ct: t) : WordSet.t =
  match ct with
  | Node (d, rej, acc) -> WordSet.union (distinguishers rej) (distinguishers acc)
                          |> WordSet.add d
  | Leaf s ->  WordSet.empty

(** [split ct oracle access newaccess dist] splits the leaf labeled by [access]
    using a new internal node with distinguishing suffix [dist], leading to two
    new leafs, labeled by [access] and [newaccess] *)
let rec split (ct: t) (oracle) (access: Word.t) (newaccess: Word.t) (dist: Word.t) =
  let recurse subtree = split subtree oracle access newaccess dist in
  match ct with
  | Node (d, rej, acc) -> if oracle access then
                            Node (d, rej, recurse acc)
                          else
                            Node (d, recurse rej, acc)
  | Leaf s -> let () = assert (s = access) in
              if oracle (Word.concat access dist) then
                Node (dist, Leaf newaccess, Leaf access)
              else
                Node (dist, Leaf access, Leaf newaccess)

(** [cex_update ct oracle alpha cex] returns a new classification tree based on
    incorporating the counterexample word [cex] *)
let cex_update (ct: t) (oracle: Word.t -> bool) (alpha: Alphabet.t) (cex: Word.t) =
  (* Get the hypothesis machine *)
  let hyp = hypothesis ct oracle alpha in
  let get_state w = Dfa.(steps hyp (get_start hyp) w) in

  (* Build a map from states in the hypothesis machine to corresponding prefix
     words at the leaves of [ct] *)
  let state_to_label w = StateMap.add (get_state w) w in
  let hyp_states = WordSet.fold state_to_label (accessors ct) StateMap.empty in

  (* Find the first prefix of cex where the hypothesis machine lands in a
     different state than sifting does *)
  (* let () = Word.to_string alpha cex |> Printf.printf "Doing %s:\n%!" in *)
  let prefix, d = List.find_map (fun p -> 
    (* let () = Word.to_string alpha p |> Printf.printf "%s\n%!" in *)
    let sifted = sift ct oracle p in
    let machine = StateMap.find Dfa.(steps hyp (get_start hyp) p) hyp_states in
    if sifted = machine then
      None
    else
      Some (p, distinguish ct oracle sifted machine)) (Word.prefixes cex) |> Option.get in

  (* [lastgood] is [prefix] less its last letter *)
  let lastgood, x = match List.rev prefix with
  | [] -> failwith "Faulty string cannot be empty string"
  | x::lastgoodrev -> List.rev lastgoodrev, x in

  split ct oracle (sift ct oracle lastgood) lastgood (Word.concat [x] d)
