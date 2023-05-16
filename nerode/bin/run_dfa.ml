open Core
open Nerode
open Intalph

let rec to_string lst = match lst with
  | s::t -> (Intalph.to_string s) ^ (to_string t)
  | [] -> ""

let print_test dfa str =
  (if Intrx.Dfa.accepts dfa str then "+" else "-") |>
  Printf.printf "%s,%s\n" (to_string str)

let () =
  let sysargs = Sys.get_argv () in

  if Array.length sysargs < 3 then failwith "usage: run_dfa <rx> <str-len>" else

  let dfa = Array.get sysargs 1 |> Parser.parse_string
                                |> Intrx.to_dfa in

  let str_len = Array.get sysargs 2 |> int_of_string in

  List.iter (enum_strings str_len) ~f:(print_test dfa)
