(* Module InputReader is only used for converting .txt files representing
   sets of positve and negative examples in an alphabet 
   for an LStar with Blanks teacher. *)

open Core
open Nerode

module SS = Set.Make(String)

let load_input fn =
  let get_ints s a =
    let (s', a') = String.fold s ~init:([],a) ~f:(fun (sl, a) x -> 
                    ((String.make 1 x)::sl, Set.add a (String.make 1 x))) in
    (List.rev s', a') in
    
  let get_alt s a =
    let strs = String.split_on_chars ~on:[' '] s in
    (* allow multiple spaces to be used as a single delimeter: *)
    let s' = if List.length strs = 0 then 
                strs 
             else
                List.filter strs ~f:(fun s -> String.length s <> 0) in
    let a' = List.fold_left s' ~init:a ~f:(fun acc s -> Set.add acc s) in
    (s', a') in

  let proc_line scan_str (p, n, a) line =
    let st = String.strip line in
    let (w, a') = scan_str (String.drop_suffix st 2) a in
    match String.suffix st 1 with
    | "+" -> (w::p, n, a')
    | "-" -> (p, w::n, a')
    | _ -> failwith ("Malformed input string: " ^ st) in

  let lines = In_channel.read_lines fn in
  let scan_str = match lines with
             | [] -> failwith "Input file was empty"
             | line::_ -> if String.contains line ';' then get_alt
                                                      else get_ints in
  let p_str, n_str, a_set = 
          List.fold_left lines ~init:([], [], SS.empty) ~f:(proc_line scan_str) in

  (* Remove "X" which has special meaning (wildcard) *)
  let a_set' = Set.remove a_set "X" |> Set.elements  in
  let alpha = if List.length a_set' = 0 then 
                Alphabet.intalph 2
              else
                Array.of_list a_set' |> Alphabet.of_string_array in

  let f = Word.words_of_strings alpha in
  (List.concat (List.map ~f p_str),
   List.concat (List.map ~f n_str), 
   alpha)
