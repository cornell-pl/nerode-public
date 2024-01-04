(** Implementation of L* with Blanks algorithm. *)

open Core
open Teacher
open ActiveLearner
open Stdlib
open Nerode
open ObsTbl

module TWordSet = Teacher.WordSet

module Make (Teacher : Teacher) (Worklist : Worklist.WorklistSig with type contents = ObsTbl.t) : 
  ActiveLearner with type teacher = Teacher.t = struct
  let learntime = ref 0.0
  let z3_time = ref 0.0
  let conjectures = ref 0
  let conjecture_time = ref 0.0
  let success_fill = ref 0
  let fail_fill = ref 0
  let popped_items = ref 0
  let cols_update_time = ref 0.0
  let unclosed_prefill = ref 0

  let cumul_learntime = ref 0.0
  let cumul_z3_time = ref 0.0
  let cumul_conjectures = ref 0
  let cumul_conjecture_time = ref 0.0
  let cumul_success_fill = ref 0
  let cumul_fail_fill = ref 0
  let cumul_popped_items = ref 0
  let cumul_cols_update_time = ref 0.0
  let cumul_unclosed_prefill = ref 0
  (* let real_z3_time = ref 0.0 *)
  (* let real_z3_queries = ref 0   *)

  let accum_metrics () =
    cumul_learntime := !cumul_learntime +. !learntime;
    cumul_z3_time := !cumul_z3_time +. !z3_time;
    cumul_conjectures := !cumul_conjectures + !conjectures;
    cumul_conjecture_time := !cumul_conjecture_time +. !conjecture_time;
    cumul_success_fill := !cumul_success_fill + !success_fill;
    cumul_fail_fill := !cumul_fail_fill + !fail_fill;
    cumul_popped_items := !cumul_popped_items + !popped_items;
    cumul_cols_update_time := !cumul_cols_update_time +. !cols_update_time;
    cumul_unclosed_prefill := !cumul_unclosed_prefill + !unclosed_prefill

  let reset_metrics () =
    learntime := 0.0;
    z3_time := 0.0;
    conjectures := 0;
    conjecture_time := 0.0;
    success_fill := 0;
    fail_fill := 0;
    popped_items := 0;
    cols_update_time := 0.0;
    unclosed_prefill := 0

  let print_metrics size query_count () =
    if CliOpt.csv () then
      Printf.printf "%d,%d,%d,%d,%f,%f,%f\n%!"
                    size
                    query_count
                    !popped_items
                    !conjectures
                    !conjecture_time
                    !z3_time
                    !learntime
    else
      begin
      Printf.printf "Dfa_size:\t%d\n%!" size;
      Printf.printf "Popped_Items:\t%d\n%!" !popped_items; (*# of tables popped 
        from the worklist and passed through algorithm. Tables that produce 
        counterexample and are put back in head of worklist are counted twice*)
      Printf.printf "Query Count:\t%d\n%!" query_count;
      Printf.printf "Conjectures:\t%d\n%!" !conjectures;
      Printf.printf "Conjecture_Time:\t%f\n%!" !conjecture_time; (*table_to_dfa+conjecture call to teacher*)
      Printf.printf "Cols_Update_Time:\t%f\n%!" !cols_update_time; (*time to add
        new columns to tables in worklist*)
      Printf.printf "BlankSMT_Time:\t%f\n%!" !z3_time;
      Printf.printf "Learn Time:\t%f\n%!" !learntime;
      end

  let print_cumul_metrics () = 
    if CliOpt.csv () then
      () (* If we're going to csv, then the consumer can sum easily enough anyway *)
    else
      begin
      Printf.printf "Total Popped_Items:\t%d\n%!" !cumul_popped_items; (*# of tables popped 
        from the worklist and passed through algorithm. Tables that produce 
        counterexample and are put back in head of worklist are counted twice*)
      Printf.printf "Total Conjectures:\t%d\n%!" !cumul_conjectures;
      Printf.printf "Total Conjecture_Time:\t%f\n%!" !cumul_conjecture_time; (*table_to_dfa+conjecture call to teacher*)
      Printf.printf "Total Cols_Update_Time:\t%f\n%!" !cumul_cols_update_time; (*time to add
        new columns to tables in worklist*)
      Printf.printf "Total BlankSMT_Time:\t%f\n%!" !cumul_z3_time;
      Printf.printf "Total Learn Time:\t%f\n%!" !cumul_learntime
      end

  module RowsSet = Set.Make(RowLabels)
  module EntryMap = WordMap

  type teacher = Teacher.t
  
  let query teacher (w : Word.t) : ObsTbl.entry = 
    match Teacher.query teacher w with
    | None -> Blank
    | Some true -> True
    | Some false -> False
  
  (*returns [true] if [w1] and [w2] not distinguishable by an added suffix.
  i.e [true] if the teacher can't show they're in different myhill-nerode classes*)
  let distinguish_conc teacher (w1: Word.t) (w2: Word.t) : bool =
    match Teacher.distinguish_concrete teacher w1 w2 with
    | None -> false
    | _ -> true
  
  let redisplay wl uncpre conj frac tbl : unit = 
    if CliOpt.verbose () then 
      begin 
        Printf.printf "Popped:[%d] Unclosed pre-fill:[%d] Conj:[%d] 
          Perc filled:[%.2f] UpperTblSize:[%d]\n%!" 
          wl uncpre conj frac (tbl |> ObsTbl.upper_row_labels |> RowLabels.cardinal);
        ObsTbl.print_table tbl
      end  

  (** helper for [lster_blanks]. Returns a worklist, entry map pair.
  The former is a worklist with [wl] plus, for each row in [rows_w_letter] 
  (or for each row in [unsat_cores] if using unsat core optimization), 
  the table [tbl] plus the row is added to the worklist, provided its rows are
  not already in [rows_history] and a table with the same rows isn't already in
  worklist [wl]. 
  The returned entry list contains the bindings of [e_map] plus any new bindings
  from queries to the teacher when building the new tables to add to [wl]*)
  let update_worklist (rows_w_letter: RowLabels.t) (unsat_core: WordSet.t option) get_entry 
    (tbl: ObsTbl.t) rows_history (teacher: Teacher.t) wl e_map = 
      let wl_tl = Worklist.tail wl in

      (*words from lower rows that will be moved up depending on optimizations*)
      let words = if CliOpt.uniq_on () then
        (*Case where using word-ordering to prevent duplication*)
        let lowerbound = ObsTbl.max_upper_row_label tbl in
        let upperbound = match unsat_core with
                         | Some u -> WordSet.max_elt_opt u
                         | _ -> None in
        (* Printf.printf "Bounding %b\n" (upperbound = None); *)
        RowLabels.filter (fun s -> Word.compare s lowerbound > 0 &&
                                   match upperbound with
                                   | None -> true
                                   | Some u -> Word.compare s u <= 0) rows_w_letter
      else if CliOpt.unsat_cores_on () then (*using unsat-cores optimization*)
        match unsat_core with
        | None -> failwith "No unsat core"
        | Some uc when WordSet.is_empty uc -> ObsTbl.print_table tbl;
                                              failwith "Empty unsat core!"
        | Some uc -> uc (*select only lower rows that are in unsat core to move up*)
      else (*default case: all lower rows get moved up into their own table*)
        rows_w_letter in
      
      let words' = if CliOpt.d_conc_on () then
        (*using strict distiguish query (distinguishes between + and -, 
        but not +/- and blank) to narrow down tables to add*)
        let distinguishable_from_upper (w: Word.t) =
          let upper_lst = RowLabels.elements (ObsTbl.upper_row_labels tbl) in
          match List.find_opt (fun s -> not (distinguish_conc teacher w s)) upper_lst with
          | None -> true
          | _ -> false  in
        match RowLabels.filter distinguishable_from_upper words with
          | s when WordSet.is_empty s -> words
          | s -> WordSet.choose s |> WordSet.singleton
      else
        words in

      (*folding over lower rows that we're moving up to make tables, that we
      enqueue to worklist, and simultaneously update the entry map [e_map]*)
      RowLabels.fold (fun sa (wl_acc, em_acc) -> 
          let rows_sa = ObsTbl.upper_row_labels tbl |> RowLabels.add sa in
          (*checks [rows_history] to see if this table 
          has already passed through worklist or is in worklist*)
          if RowsSet.mem rows_sa rows_history 
            || Worklist.exists 
              (fun tab -> ObsTbl.upper_row_labels tab |> RowLabels.equal rows_sa ) wl
          then (*table has been enqueued already in past, skip to next one*)
            begin
              if CliOpt.uniq_on () then failwith "impossible dup!\n"
              else if CliOpt.verbose () then Printf.printf "Dup avoided!\n%!"
              else ();
              wl_acc, em_acc
            end
          else (*make new table with [sa] moved up, and update entry map*)
            let t', em_acc' = ObsTbl.add_row get_entry em_acc sa tbl in 
            if CliOpt.verbose () then begin
              Printf.printf "Enqueued to back of wl:\n%!"; 
              ObsTbl.print_table t' end;
            Worklist.enqueue t' wl_acc, em_acc')
        words' (wl_tl, e_map)
  
  (*using distinguish as replacement for validity query for teacher; 
  terminates if the teacher has a finite set of example strings*)
  let conjecture_by_distinguish table teacher : Word.t option = 
    let cols = ObsTbl.col_labels table |> ColLabels.elements |>  TWordSet.of_list in
    ObsTbl.equivalent_pairs table |> List.fold_left 
    (fun acc (urow, lrow) -> match acc with 
      | None -> Teacher.distinguish teacher urow lrow cols
      | counterex -> counterex) None

  (**Lstar with blanks main loop*)
  let rec lstar_blanks (alpha: Alphabet.t) ((wl : Worklist.t), rows_history, teacher, g_cols, e_map) : Dfa.t = 
    let hd_t = Worklist.head wl in

    let tbl, e_map = if CliOpt.global_e () then
      (*add new suffixes in global [g_cols] to popped table and update global [e_map]*)
      let cur_cols = ObsTbl.col_labels hd_t in 
      let new_cols = ColLabels.diff g_cols cur_cols in
      ObsTbl.add_cols (query teacher) e_map new_cols hd_t
    (*columns not global, no need to update table or entry map*)
    else hd_t, e_map in

    (*update metrics*)
    let () = popped_items := !popped_items + 1 in
      redisplay 
        !popped_items 
        !unclosed_prefill
        !conjectures 
        ((!success_fill |> float_of_int) /. 
          (!success_fill + !fail_fill |> float_of_int) *. 100.) 
        tbl;

    let urows = ObsTbl.upper_row_labels tbl in
    (*add S (upper rows of table) to [rows_history] so same table not enqueued again*)
    let rows_history' = RowsSet.add urows rows_history in

    (*checking if table can't be closed even before filling blanks (if we don't use unsat cores)*)
    match if CliOpt.uniq_on () || CliOpt.unsat_cores_on () then None else ObsTbl.closed tbl with
    
    | Some lower_row -> 
      (*unclosed before filling blanks*)
      unclosed_prefill := !unclosed_prefill + 1;
      let tbl', e_map' = ObsTbl.add_row (query teacher) e_map lower_row tbl in
      let wl' = Worklist.(wl |> tail |> enqueue tbl') in
      if CliOpt.verbose () then begin 
        Printf.printf "Failed to close even with blanks due to \
          row [%s]. Enqueued to back of wl:\n%!" (Word.to_string alpha lower_row);
        ObsTbl.print_table tbl' end;
      lstar_blanks alpha (wl', rows_history', teacher, g_cols, e_map')

    | None -> (*attempt to fill balnks with solver*)
      let start = Core_unix.gettimeofday () in
      let filled = Solver.fill_blanks tbl in
      let () = z3_time := !z3_time +. (Core_unix.gettimeofday () -. start) in
      
      match filled with

        | None -> 
          fail_fill := !fail_fill + 1;
          let rows_w_letter = ObsTbl.lower_row_labels tbl in
          (*add new tables to worklist and update entry map*)
          let wl_updated, e_map' =
            if CliOpt.unsat_cores_on () then (*only move up rows in unsat core*)
              begin
              let unsat_core = Solver.get_unsats () in
              update_worklist rows_w_letter (Some unsat_core) (query teacher) tbl rows_history' teacher wl e_map
              end
            else (*no unsat core optimization, move up all rows*)
              update_worklist rows_w_letter None (query teacher) tbl rows_history' teacher wl e_map in
          lstar_blanks alpha (wl_updated, rows_history', teacher, g_cols, e_map')

        | Some obs -> (*table successfully filled in as [obs]*)
          success_fill := !success_fill + 1;
          if CliOpt.verbose () then 
            begin
              Printf.printf "Filling blanks was successful:\n%!";
              ObsTbl.print_table obs;
              Printf.printf "Making dfa...\n%!";
            end;
          let () = conjectures := !conjectures + 1 in
          let start = Core_unix.gettimeofday () in 
          let dfa = ObsTbl.to_dfa obs in (*convert table to dfa*)
          if CliOpt.verbose () then begin
              Dfa.print dfa;
              Printf.printf "Conjecturing dfa...\n%!" end;
          let result = if CliOpt.distinguish_on () 
            then conjecture_by_distinguish obs teacher (*distinguish query*)
            else Teacher.conjecture teacher dfa in (*default validity query*)
          let () = conjecture_time := 
            !conjecture_time +. (Core_unix.gettimeofday () -. start) in 
          
          match result with

          | None -> (*no counterexamples from conjecture, minimal dfa found*)
            if CliOpt.distinguish_on () 
              (*checking to see if distinguish_blanks produced a correct DFA*)
              then begin match Teacher.conjecture teacher dfa with
                | Some cex -> failwith "Distinguish_blanks returned an incompatible DFA! Uh-oh!"
                | None -> dfa
                end
            else dfa

          | Some counterex ->
            if CliOpt.verbose () then 
              Printf.printf "Conjecture rejected due to counterexample [%s]:\n%!"
                (Word.to_string alpha counterex);
            let start = Core_unix.gettimeofday () in
            let e = ObsTbl.col_labels tbl in
            let new_suffixes = counterex 
              |> Word.suffixes 
              (*filter gets rid of suffixes that are already column labels in table.*)
              |> List.filter (fun w -> not (ColLabels.mem w e)) 
              |> ColLabels.of_list 
            in
            let wl', e_map'' = if CliOpt.global_e () 
              then (*Suffix-set shared, so only updating global E [g_cols]*)
                wl, e_map
              else (*adding suffixes of counterexample and 
              corresponding entries of columns to popped table*)
                let tbl', e_map' = 
                  ObsTbl.add_cols (query teacher) e_map new_suffixes tbl in 
                (*if WorklistDefault, [enquque_high_pri] adds to front of list*)
                Worklist.(enqueue_front tbl' (tail wl)), e_map'
            in
            (*[g_cols] updated for case where we do suffix-set sharing*)
            let g_cols' = ColLabels.union g_cols new_suffixes in
            let () = cols_update_time := 
              !cols_update_time +. (Core_unix.gettimeofday () -. start) in 
            lstar_blanks alpha (wl', rows_history, teacher, g_cols', e_map'')
  
  (** Entry point of L* with blanks algorithm. *)
  let learn (alpha : Alphabet.t) teacher = 
    let () = reset_metrics () in
    let learnstart = Core_unix.gettimeofday () in

    (* First 1x1 table and initial entry map containg only epsilon *)
    let (tbl, e_map) : ObsTbl.t * ObsTbl.entry EntryMap.t =
      ObsTbl.init_epsilon alpha (query teacher) in

    (* Worklist is implemented either as a list of tables (type ObsTbl.t) in 
    default setting, or as a priority queue of tables in priority queue 
    hueristic optimization*)
    let wl : Worklist.t = Worklist.(empty |> enqueue tbl) in

    (* [rows_history] is represented as a Set of RowLabels, no duplicates. As 
    implemented now, RowsHistory maintains a history of rows of tables that 
    have been in the worklist and have failed to be closed in the algorithm, 
    to make sure tables with the same rows aren't added to the worklist again. *)
    let rows_history = RowsSet.empty in

    (* [g_cols] is a set of words of type Word.t, no duplicates. It serves as 
    the global suffix-set when the algorithm utilizes suffix-set sharing across
    tables*)
    let g_cols = ColLabels.empty in

    let dfa = lstar_blanks alpha (wl, rows_history, teacher, g_cols, e_map) in
    let () = learntime := (*total amount of time it took to learn dfa*)
              !learntime +. (Core_unix.gettimeofday () -. learnstart) in
    let () = print_metrics (Dfa.size dfa) (Teacher.number_queries teacher) () in
    let () = accum_metrics () in
    let () = print_cumul_metrics () in
    dfa
end
