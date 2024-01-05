(** Some unit tests for the modules in Nerode. *)

open Nerode

(* More tests might be appropriate..*)

let w001 = Word.of_intlist [0;0;1]
let w10 = Word.of_intlist [1;0]

let rx001 = Rx.of_word w001
let rx10 = Rx.of_word w10

let rx001star = Rx.star rx001
let rx_empty = Rx.Empty

let dfa_empty = Dfa.of_rx (Alphabet.intalph 2) rx_empty
let dfa001 = Dfa.of_rx (Alphabet.intalph 2) rx001
let dfa10 = Dfa.of_rx (Alphabet.intalph 2) rx10

let nfa001 = Dfa.to_nfa dfa001
let nfa10 = Dfa.to_nfa dfa10

(* DFA tests *)
let dfa_is_empty () =
  Alcotest.(check bool) "same bool" true (Dfa.is_empty dfa_empty)

let dfa_not_empty () =
  Alcotest.(check bool) "same bool" false (Dfa.is_empty dfa001)

let dfa_rep () =
  Alcotest.(check bool) "same bool" true (Dfa.rep dfa001 = w001)

let dfa_accept () =
  Alcotest.(check bool) "same bool" true (Dfa.accept dfa001 w001)

let dfa_reject () =
  Alcotest.(check bool) "same bool" false (Dfa.accept dfa001 w10)

let union_accept_001 () =
  Alcotest.(check bool) "same bool" true (Dfa.accept (Dfa.union dfa001 dfa10) w001)

let union_accept_10 () =
  Alcotest.(check bool) "same bool" true (Dfa.accept (Dfa.union dfa001 dfa10) w10)

let sym_diff_empty () =
  Alcotest.(check bool) "same bool" false (Dfa.symdiff dfa001 dfa10 |> Dfa.is_empty)

let intersect_empty () =
  Alcotest.(check bool) "same bool" true (Dfa.intersect dfa001 dfa10 |> Dfa.is_empty)

let diff_accept_001 () =
  Alcotest.(check bool) "same bool" true (Dfa.accept (Dfa.diff dfa001 dfa10) w001)

let diff_reject_10 () =
  Alcotest.(check bool) "same bool" false (Dfa.accept (Dfa.diff dfa001 dfa10) w10)

let () =
  Alcotest.run "Dfa"
  [
    ( "empty",
      [
        Alcotest.test_case "empty" `Quick dfa_is_empty;
        Alcotest.test_case "not empty" `Quick dfa_not_empty;
      ]
    );
    ( "rep",
      [
        Alcotest.test_case "rep" `Quick dfa_rep;
      ]
    );
    ( "accept",
      [
        Alcotest.test_case "accept" `Quick dfa_accept;
        Alcotest.test_case "reject" `Quick dfa_reject;
      ]
    );
    ( "union",
      [
        Alcotest.test_case "accept 001" `Quick union_accept_001;
        Alcotest.test_case "accept 10" `Quick union_accept_10;

      ]
    );
    ( "sym-diff",
      [
        Alcotest.test_case "sym-diff empty" `Quick sym_diff_empty;
      ]
    );
    ( "intersect",
      [
        Alcotest.test_case "intersect empty" `Quick intersect_empty;
      ]
    );
    ( "diff",
      [
        Alcotest.test_case "diff accept 001" `Quick diff_accept_001;
        Alcotest.test_case "diff reject 10" `Quick diff_reject_10;
      ]
    );
  ]
