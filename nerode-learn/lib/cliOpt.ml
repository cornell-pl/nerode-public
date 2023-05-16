(** Module CliOpt is only used for managing global variables for command line
   options. *)

(* Verbose *)

(** Verbose settings are Off, On (for verbose output), Latex (for latex-formatted tables), CSV (for summary data) *)
type v = Off | On | Latex | Csv
let verb_setting: v ref = ref Off
let set_verb (v:v): unit =
  verb_setting := v
let verbose_log if_on if_latex arg: unit =
  match !verb_setting with
  | On -> if_on arg
  | Latex -> if_latex arg
  | _ -> ()
let csv () = (!verb_setting = Csv)

(** Return the current setting for the verbose flag *)
let verbose () = (!verb_setting = On)

(* Last guess / Word ordering / (i.e., Unique tables) *)
let uniq = ref false
let set_uniq (b:bool): unit =
  uniq := b
let uniq_on () = !uniq

(* Unsat cores optimization *)
let unsat_cores = ref false
let set_unsat_cores (b:bool): unit =
  unsat_cores := b

(** Return whether the unsat cores option is set. *)
let unsat_cores_on () = !unsat_cores

let d_conc = ref false
let set_d_conc (b:bool): unit =
  d_conc := b
let d_conc_on () = !d_conc

(* Conjecturing with distinguish queries instead*)
let distinguish = ref false
let set_distinguish (b:bool): unit =
  distinguish := b
let distinguish_on () = !distinguish

(* Global E maintained as argument in main loop *)
let ge = ref false
let set_ge (b:bool): unit =
  ge := b

(** Return whether the global E option is set. *)
let global_e () = !ge

(*Worklist implemented as a priority queue with heuristic prioritization*)
let pq = ref false
let set_pq (b:bool): unit = 
  ge := b

(** Return whether the heuristic prioritization option is set. *)
let pqueue () = !pq
