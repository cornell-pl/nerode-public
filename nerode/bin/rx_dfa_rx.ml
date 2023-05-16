open Core
open Nerode

let () =
  let sysargs = Sys.get_argv () in
  let arg = Array.get sysargs 1 in
  let rx = (Parser.parse_string arg) in
  let a = Alphabet.intalph 2 in
  begin
    Printf.printf "Your rx: %s\n%!" (Rx.to_string a rx);

    let dfa: Dfa.t = Dfa.of_rx a rx in

    Printf.printf "Dfa: ";
    Dfa.print dfa;

    (*
    Printf.printf "running 011: %B\n%!" (Intrx.Dfa.accepts dfa [0; 1; 1]);
    Printf.printf "running 10: %B\n%!" (Intrx.Dfa.accepts dfa [1; 0]);
    *)

    Printf.printf "\nDfa->Nfa:\n";
    IntNfa.print (Dfa.to_nfa dfa);

    Printf.printf "\nDfa->Rx: \n%s\n" (Dfa.to_rx dfa |> Rx.to_string a);

    (*
    Printf.printf "Representative: %s\n" (Intrx.Dfa.representative dfa)
    *)
  end
