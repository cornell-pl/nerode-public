type symbol = Alphabet.symbol
type word = Alphabet.word

type t = {
  alpha: Alphabet.t;
  trans: int array array; (* start state is 0. 1st dim: states, 2nd dim: symbols *)
  final: bool array
}
type state = int

module StateSet = Set.Make(Int)

type 'a regular = { eq: 'a -> 'a -> bool;
                    d: symbol -> 'a -> 'a;
                    e: 'a -> bool }

type 'a trans = 'a * symbol * 'a

let idx (eq: 'a -> 'a -> bool) (lst: 'a list) (q: 'a) =
  let rec idxr eq lst q i =
    match lst with
    | [] -> failwith "State wasn't found :("
    | x::tail -> if eq x q then i else idxr eq tail q (i+1) in
  idxr eq lst q 0

let rec dfa_goto (alpha: Alphabet.t) (r: 'a regular) (q: 'a) (c: symbol)
                 (states: 'a list) (delta: ('a trans) list) =
  let qc = r.d c q in
  if List.exists (r.eq qc) states then
    (states, (q, c, qc)::delta)
  else
    (* Add qc _second_ to maintain the invariant that 0 is the start state *)
    dfa_explore alpha r ((List.hd states)::qc::(List.tl states)) ((q, c, qc)::delta) qc

and dfa_explore (alpha: Alphabet.t) (r: 'a regular)
                (states: 'a list) (delta: ('a trans) list) (q: 'a) = 
  Alphabet.fold (fun (s,d) c -> (dfa_goto alpha r q c s d)) (states, delta) alpha

let mk_dfa (r: 'a regular) (alpha: Alphabet.t) (start:'a) : t =
  let m = Alphabet.size alpha in
  let (states, trans) = dfa_explore alpha r [start] [] start in
  let n = List.length states in
  let arr = Array.make_matrix n m 0 in
  let () = List.iter (fun (s1, x, s2) ->
    arr.(idx r.eq states s1).(Alphabet.sym_to_int x) <- (idx r.eq states s2)) trans in
  let f = List.map r.e states |> Array.of_list in
  {
    alpha = alpha;
    trans = arr;
    final = f
  }

let compare_states = Int.compare let size (dfa: t) = Array.length dfa.trans
let get_alpha (dfa: t) = dfa.alpha
let get_start (dfa: t) = 0

let step (dfa: t) (s: state) (a: symbol) =
  dfa.trans.(s).(Alphabet.sym_to_int a)

let accepting (dfa: t) (s: state) =
  dfa.final.(s)

let accept (dfa: t) (w: word) =
  let qf = List.fold_left (step dfa) 0 w in
  dfa.final.(qf)

let validate (d: t) (pos: word list) (neg: word list) : bool =
  not (List.exists (fun s -> not (accept d s)) pos) &&
  not (List.exists (fun s -> accept d s) neg)

let is_empty (d: t) : bool =
  (* This implementation relies on the invariant that the dfa is trim, which
     is true if it was constructed using mk_dfa *)
  Array.exists Fun.id d.final |> not

let rep (d: t) : word =
  (* Peform a BFS of the DFA to find a representative string *)
  let rec r (d: t) (s: (word*state) list) (v: StateSet.t) =
    match s with
    | (w,q)::tail -> if d.final.(q) then
                       w
                     else
                       let tr = Array.to_list (Array.mapi (fun i s -> 
                         (Alphabet.sym_of_int i,s)) d.trans.(q)) in
                       let next = List.filter_map (fun (a, qa) -> 
                         if StateSet.mem qa v then
                           None
                         else
                           Some (w@[a], qa)
                       ) tr in
                       let v' = List.fold_left (fun a (_,n) -> StateSet.add n a) v next in
                       r d (tail@next) v'
    | _ -> failwith "rep: Stack unexpectedly emptied" in
  r d [([],0)] StateSet.empty

let complement (d: t) =
  { d with final = Array.map not d.final }

let print (d: t) =
  let sw = 3 in
  let spacer = String.init sw (fun x -> ' ') in
  let shortline = String.init sw (fun x -> '-') in
  let a = Alphabet.to_string d.alpha in
  let () = Printf.printf "\n%s| %s" spacer a in
  let alen = String.length a in
  let sym_lens = Alphabet.symbols d.alpha |> List.map (Alphabet.sym_to_string d.alpha) 
                 |> List.map String.length |> Array.of_list in
  let longline = String.init (alen+1) (fun x -> '-') in
  let () = Printf.printf "\n%s+%s\n" shortline longline in
  Array.iteri (fun i r ->
    Printf.printf ("%2d | ") i;
    Array.iteri (fun j s -> Printf.printf ("%d%s") s (String.make sym_lens.(j) ' ')) r;
    Printf.printf (if d.final.(i) then "*\n%!" else "\n%!")
  ) d.trans

module type Determ  = sig
  module N : Nfa.N
  val determinize : N.t -> t
end

module Determinizer (S : Nfa.State) = struct
  module N = Nfa.Make(S)
  (* The following (eq, d, e) implement the Nfa->Dfa subset construction *)
  type npair = N.t * N.StateSet.t
  let eq ((_,q0):npair) ((_,q1):npair) = N.StateSet.compare q0 q1 = 0
  let d (x: symbol) ((n,q):npair) = (n, N.next n q x)
  let e ((n,q):npair) = N.contains_final n q
  let reg = { eq=eq; d=d; e=e }
  let determinize (nfa: N.t) =
    mk_dfa reg (N.get_alpha nfa) (nfa, N.get_start nfa)
end
module IntDeterm = Determinizer(IntNfa.FInt)

let to_nfa (dfa: t) : IntNfa.t =
  let n = Array.length dfa.final in
  let final = List.filter (fun i -> dfa.final.(i)) (List.init n Fun.id) in
  let trans = Array.fold_left (fun a1 (i,r) ->
    Array.fold_left (fun a2 (j,x) ->
      (i, Nfa.Char (Alphabet.sym_of_int j), x)::a2
    )  a1 (Array.mapi (fun j x -> (j, x)) r)
  ) [] (Array.mapi (fun i r -> (i, r)) dfa.trans) in
  IntNfa.mk_nfa dfa.alpha [0] final trans

let minimize (dfa: t) : t =
  (* Brzozowski's algorithm for state minimization *)
  to_nfa dfa |> IntNfa.reverse |> IntDeterm.determinize
  |>  to_nfa |> IntNfa.reverse |> IntDeterm.determinize

let of_rx (alpha: Alphabet.t) (rx: Rx.t) : t =
  mk_dfa {eq = Rx.equiv; d = Rx.d; e = Rx.e} alpha rx
let to_rx (dfa: t) : Rx.t = to_nfa dfa |> IntNfa.to_rx

(* Crossproduct of DFAs -- for intersect, union, symdiff, diff*)
let dfa_op = {
  eq = (fun (_,a,_,b) (_,c,_,d) -> (a=c && b=d));
  d = (fun a (d0,s0,d1,s1) -> (d0, step d0 s0 a, d1, step d1 s1 a));
  e = (fun x -> failwith "unimplemented")
}

let intersect_e (d0, s0, d1, s1) = (accepting d0 s0) && (accepting d1 s1)
let union_e (d0, s0, d1, s1) = (accepting d0 s0) || (accepting d1 s1)
let symdiff_e (d0, s0, d1, s1) = (accepting d0 s0) <> (accepting d1 s1)
let diff_e (d0, s0, d1, s1) = (accepting d0 s0) && not (accepting d1 s1)

let mk_dfa_combinator e =
  fun d0 d1 -> mk_dfa {dfa_op with e=e} (get_alpha d0) (d0, get_start d0, d1, get_start d1)

let intersect = mk_dfa_combinator intersect_e
let union = mk_dfa_combinator union_e
let symdiff = mk_dfa_combinator symdiff_e
let diff = mk_dfa_combinator diff_e

let equiv (d0: t) (d1: t) = symdiff d0 d1 |> is_empty
