(* module type State = sig
  type t
  module StateSet : Set.S with type elt = t
  val compare : t -> t -> int
  val to_string : t -> string
  val fresh : StateSet.t -> t
end *)
open String
type word = Alphabet.word

let str_to_clst s = List.init (String.length s) (String.get s)

module Output = struct
  type elt = Alphabet.symbol (*maybe should be different*)
  type t = bool option (*maybe should be bool option*)

  (*converting parsed output to bool options, failing if output alphabet is not {0,1}*)
  let str_to_output s : t = 
    if String.length s > 1 
      then 
        begin failwith "the mealy implementation does not currently handle output alphabets larger than 2";
        end
    else
    match s with
      | "0" -> Some false
      | "1" -> Some true
      | "-" -> None
      | c -> failwith ("unexpected value " ^ c
                                            ^ " in output string") 

  (* let compare = failwith "TODO" *)
end

type state = string
module StateSet = Set.Make(Int)
module StateMap = Map.Make(Int)
(* module CharMap = Map.S with type key = nsymbol *)
module StartInpPair = struct
  type t = string*state
  let compare (s1, st1) (s2, st2) = 
    if st1 = st2 then String.compare s1 s2 else String.compare st1 st2
end
module TransMap = Map.Make(StartInpPair)

type transitions = (state*Output.t) TransMap.t

type t = {
  start : state;
  alpha : Alphabet.t;
  in_len : int; (*number of characters used per transition*)
  transitions : transitions;
  } 
  
type parseddec = {
  in_len : int option;
  out_len : int option;
  ntrans : int option;
  nstates : int option;
  start : string option
  }

type parsedtrans = ((string * string) * (string * string)) list

type parsed = (parseddec) * parsedtrans

let instr_to_strs instr = 
  let rec f acc rem_str = 
    match rem_str with
    | "" -> acc
    | s when s.[0] = '-' -> 
        f (List.fold_left (fun newacc si -> (si^"0")::(si^"1")::newacc) [] acc) String.(sub s 1 ((length s)-1))
    | s -> f (List.map (fun si -> si^(String.make 1 s.[0])) acc) String.(sub s 1 ((length s)-1))
  in
  f [""] instr

(*making the transitions assuming that the alphabet is 2^input_len *)
let mk_trans (parsedtr: parsedtrans) nstates : transitions =  
  let f mapacc ((inp, st1), (st2, outp)) = 
    let inputs = instr_to_strs inp in
    List.fold_left 
      (fun macc istr -> 
        TransMap.add (istr, st1) (st2, Output.str_to_output outp) macc) 
      mapacc inputs
  in
  List.fold_left f TransMap.empty parsedtr

let rec pow a = function
| 0 -> 1
| 1 -> a
| n -> 
  let b = pow a (n / 2) in
  b * b * (if n mod 2 = 0 then 1 else a)

let parsed_to_mealy 
  ((parseddec, parsedtr) : parsed) : t = 
  begin 
    let left, right = parseddec.ntrans |> Option.get, List.length parsedtr in
      if left <> right
      then Printf.printf ".p = %i not same as # transitions read = %i \n%!" left right;
  end;
  let start = match parseddec.start with
  | Some st -> st
  | None -> "1" (*TODO: figure out what default start state is when unspecififed*)
  in
  match parseddec.out_len with
  | Some i when i <> 1 -> failwith "the mealy implementation does not currently handle output alphabets larger than 2";
  | _ -> 
  let ilen = parseddec.in_len |> Option.get in
  let transitions = mk_trans parsedtr (parseddec.nstates |> Option.get) in
  {
    start = start;
    (*Using intalph for alphabet keeping alphabet as strings
    representing different bitvectors like in .kiss files now, migth consider sing intalph for alphabet instead.
    NOTE: sometimes not all the symbols in the alphabet are necessary, 
    but the kiss files don't specify for us in those cases as far as I can tell*)
    alpha = begin
      instr_to_strs (String.make ilen '-') 
      |> Array.of_list |> Alphabet.of_string_array 
      end;
    (* Alphabet.intalph (pow 2 ilen); *)
    in_len = ilen;
    transitions = transitions
  }

let print_pmealy pm = failwith "TODO"