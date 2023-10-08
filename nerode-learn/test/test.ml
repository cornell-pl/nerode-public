open OUnit2

open Nerode
open Nerodelearn


let test name ex_output input =
  name >:: fun _ -> assert_equal ex_output input

let weps = Word.epsilon
let w1 = Word.of_intlist [0;1;2]
let w2 = Word.of_intlist [3;4;5]

let word_tests = [
  Word.(test "concat words" [0;1;2;3;4;5] (concat w1 w2 |> to_intlist));
  Word.(test "concat eps and word" [0;1;2] (concat weps w1 |> to_intlist));
  Word.(test "concat eps and word" [0;1;2] (concat w1 weps |> to_intlist));
  Word.(test "append letter" [0;1;2;0] 
    (0 |> Alphabet.sym_of_int |> append_letter w1 |> to_intlist));
  Word.(test "suffixes for epsilon" [weps] (suffixes weps));
  Word.(test "suffixes for non-empty word" [[3;4;5];[4;5];[5];[]] 
          (w2 |> suffixes |> List.map to_intlist));
]

module TI = TeacherIndifferent

let mk_teacher fn = 
  let (pos, neg, alpha) = InputReader.load_input fn in
  (*
  Printf.printf "%s\n%!" (Unix.getcwd ());
  *)
  TI.make pos neg 

let teacher1 = mk_teacher "./tomita_t1"
let teacher3 = mk_teacher "./tomita_t3"

let alpha01 = Alphabet.intalph 2

let failed_conj = function
  | Some _ -> true
  | None -> false

let aw1111 = Alphabet.w_of_ints [1;1;1;1]
let aw0 = Alphabet.w_of_ints [0]
let aw1 = Alphabet.w_of_ints [1]
let aw00 = Alphabet.w_of_ints [0;0]
let aw010 = Alphabet.w_of_ints [0;1;0]
let aw10 = Alphabet.w_of_ints [1;0]
let aw1010 = Alphabet.w_of_ints [1;0;1;0]
let aw100 = Alphabet.w_of_ints [1;0;0]
let aweps = Alphabet.w_of_ints []

let teacher_tests = [
  TI.(test "teacher query pos" (Some true) (query teacher1 aw1111));
  TI.(test "teacher query neg" (Some false) (query teacher1 aw00));
  TI.(test "teacher query blank" None (query teacher1 aw010));
  TI.(test "teacher query eps" (Some true) (query teacher1 aweps));
  TI.(test "teacher distiguish_concrete indistinguishable" (None) 
      (distinguish_concrete teacher3 aw10 aw1010));
  TI.(test "teacher distiguish_concrete distinguishable" (true) 
      (distinguish_concrete teacher3 aweps aw100 |> Option.is_some));
  TI.(test "teacher distiguish distinguishable" (true) 
      (distinguish teacher1 aw0 aw00 WordSet.empty |> Option.is_some));
]

let aword w = w |> Word.to_intlist |> Alphabet.w_of_ints
let query teacher (w : Word.t) : ObsTbl.entry = 
  match TI.query teacher (aword w) with
  | None -> Blank
  | Some true -> True
  | Some false -> False

let tbl_init1, e_map1 = ObsTbl.init_epsilon (Alphabet.intalph 2) (query teacher1)
let w_zero = Word.of_intlist [0]
let w_one = Word.of_intlist [1]

let tests =
  "test suite for nerode-learn"  >::: List.flatten [
    word_tests;
    teacher_tests;
  ]

let _ = run_test_tt_main tests


