open Core
open Nerode
open Nerodelearn

module SS = Set.Make(String)

let set_verbose v tex c = CliOpt.(set_verb (match v,tex,c with
  | true,_,_ -> On
  | _,true,_ -> Latex
  | _,_,true -> Csv
  | _,_,_ -> Off))

module WLSDef = Lstarblanks.Make(TeacherIndifferent)(Worklist.WorklistDefault)
module WLSPQ = Lstarblanks.Make(TeacherIndifferent)(Worklist.WorklistPQ)
module WLSepDef = Lstarblanks.Make(TeacherSep)(Worklist.WorklistDefault)
module WLSepPQ = Lstarblanks.Make(TeacherSep)(Worklist.WorklistPQ)

module KVD = Kv.Make(TeacherLStar)

let kv_from_rx r () =
  let alpha = Alphabet.intalph 2 in
  let teacher = Parser.parse_string r
                |> Dfa.of_rx alpha
                |> TeacherLStar.make in
  let d = KVD.learn alpha teacher in
  Dfa.print d;
  Printf.printf "Dfa size: %d\n%!" (Dfa.size d);
  Printf.printf "%s\n%!" (Dfa.to_rx d |> Rx.to_string alpha)

let learn_from_sep_rx r1 r2 () =
  let alpha = Alphabet.intalph 2 in
  let rx1 = Parser.parse_string r1 in
  let rx2 = Parser.parse_string r2 in
  let teacher = TeacherSep.make alpha rx1 rx2 in
  (*automatically use three optimizations from paper in this setting*)
  let () = CliOpt.set_pq true in
  let () = CliOpt.set_unsat_cores true in
  let () = CliOpt.set_ge true in
  let () = Solver.produce_unsats () in
  let d = WLSepPQ.learn alpha teacher in
  Dfa.print d;
  Printf.printf "Dfa size: %d\n%!" (Dfa.size d);
  Printf.printf "%s\n%!" (Dfa.to_rx d |> Rx.to_string alpha)

let learn_from_examples v tex uniq sc dist db ge pq (csv:bool) fn () =
  let () = set_verbose v false csv in (*no latex for now *)
  (*[set_ge] sets suffix-sharing between tables optimization on if [ge] true*)
  let () = CliOpt.set_ge ge in
  (*[set_uniq] enables word ordering of table row labels to prevent searching 
  duplicates if [uniq]. However, cannot use priority queue optimization with this*)
  let () = CliOpt.set_uniq uniq in
  (*[set_unsat_cores] enables optimization of only moving up lower rows in 
  unsat core of SAT solver attempt to fill in the table if [sc]*)
  let () = CliOpt.set_unsat_cores sc in
  (*[set_d_conc] permits learner to use strict distinguish query to teacher
  to further narrow down search is [dist]. Not normally provided in iMAT framework*)
  let () = CliOpt.set_d_conc dist in
  (*[set_distinguish] sets learner to use distinguish query of weakened iMAT
  as substitute for validity query to conjecture dfa if [db]*)
  let () = CliOpt.set_distinguish db in
  (*[set_pq] enables optimization of implementation of Worklist as a 
  priority queue, with tables ordered using heuristics, if [pq]*)
  let () = CliOpt.set_pq pq in
  if sc then
    Solver.produce_unsats ();
  let (pos, neg, alpha) = InputReader.load_input fn in
  let teacher = TeacherIndifferent.make pos neg in
  let learn = if CliOpt.pqueue() then WLSPQ.learn else WLSDef.learn in
  let d = learn alpha teacher in
  match CliOpt.csv () with
  | false -> begin
             Dfa.print d;
             if not (Dfa.validate d pos neg) then failwith "DFA failed validation" else
               Printf.printf "%s\n%!" (Dfa.to_rx d |> Rx.to_string alpha)
            end
  | _ -> ()

(*run algorithm on whole directory*)
let learn_from_dir_examples v tex uniq sc dist db ge pq (csv:bool) dn () = 
  let () = set_verbose v tex csv in
  let () = if CliOpt.csv () then
              Printf.printf "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
                  "filename"
                  "dfa-size"
                  "membership-queries"
                  "items-processed"
                  "conjectures"
                  "conjecture-time"
                  "z3-time"
                  "learn-time"
           else
              () in
  let file_arr = Sys_unix.readdir dn in 
  let n = Array.length file_arr in
  Array.iteri file_arr 
    ~f:(fun i fn -> 
          let () = Printf.eprintf "%s. [%d / %d files]\n%!" fn i n  in
          let () = if CliOpt.csv () then
            Printf.printf "\"%s\"," fn
          else
            Printf.printf "%s:\n%!" fn in

          learn_from_examples v tex uniq sc dist db ge pq csv (dn^"/"^fn) ())

let kvrx_cmd =
  let open Command.Spec in
  Command.basic_spec
    ~summary: "Run KV using a teacher built from a Rx"
    (empty
    +> anon ("rx" %: string))
    kv_from_rx

let lsb_sep_cmd =
  let open Command.Spec in
  Command.basic_spec
    ~summary: "Run the L* with Blanks on two regular languages"
    (empty
    +> anon ("L+" %: string)
    +> anon ("L-" %: string))
    learn_from_sep_rx

let lsb_cmd =
  let open Command.Spec in
  Command.basic_spec
    ~summary: "Run L* with Blanks on a finite example set"
    (empty
     +> flag "-v" no_arg ~doc: ("Verbose")
     +> flag "-l" no_arg ~doc: ("Latex")
     +> flag "-u" no_arg ~doc: ("Use Word-ordering to prevent investigating duplicate tables")
     +> flag "-s" no_arg ~doc: ("Use unsat cores, but *not* word-ordering (use the visited set instead)")
     +> flag "-dc" no_arg ~doc: ("Use the teacher's concrete version of `distinguish' query to improve learner")
     +> flag "-d" no_arg ~doc: ("Use teacher's `distinguish' queries as substitute for conjecture")
     +> flag "-ge" no_arg ~doc: ("Make Columns global/Global E maintained as argument in main loop")
     +> flag "-pq" no_arg ~doc: ("Use Heurisitc prioritization in Worklist")
     +> flag "-f" no_arg ~doc: ("Write data in csv format.")
     +> anon ("file" %: string))
    learn_from_examples

let lsb_dir_cmd = 
  let open Command.Spec in
  Command.basic_spec
    ~summary: "Run L* with Blanks on a whole directory of benchmarks"
    (empty
     +> flag "-v" no_arg ~doc: ("Verbose")
     +> flag "-l" no_arg ~doc: ("Latex")
     +> flag "-u" no_arg ~doc: ("Use Word-ordering to prevent investigating duplicate tables")
     +> flag "-s" no_arg ~doc: ("Use Z3 unsat cores, and *not* word-ordering (use the visited set instead)")
     +> flag "-dc" no_arg ~doc: ("Use the teacher's concrete version of `distinguish' query to improve learner")
     +> flag "-d" no_arg ~doc: ("Use teacher's `distinguish' queries as substitute for conjecture")
     +> flag "-ge" no_arg ~doc: ("Make Columns not global/Tables can have different columns")
     +> flag "-pq" no_arg ~doc: ("Use Heurisitc prioritization in Worklist")
     +> flag "-f" no_arg ~doc: ("Write data in csv format.")
     +> anon ("file" %: string))
    learn_from_dir_examples

let main : Command.t =
  Command.group
    ~summary:"Execute an automaton learning algorithm"
    [
     ("lsblanks", lsb_cmd);
     ("lsblanks-dir", lsb_dir_cmd);
     ("lsblanks-sep", lsb_sep_cmd);
     ("kv", kvrx_cmd);
    ]

let () = Command_unix.run ~version: "0.1.1" main
