(** A worklist of observation tables. *)

open ObsTbl


(*Module type for a worklist in L* with Blanks. *)
module type WorklistSig = sig

  (** The type of the contents contained within the worklist *)
  type contents
  type t

  (** [empty] returns an empty worklist*)
  val empty : t

  (** [head] returns first element of the worklist. 
  Raises a [Failure] if the worklist is empty*)
  val head : t -> contents

  (** [tail] returns the worklist without the first element.
  Raises a [Failure] if the worklist is empty*)
  val tail : t -> t
  
  (** enqueues an element to the front of the worklist*)
  val enqueue_front : contents -> t -> t

  (** adds an element to the worklist based on an ordering 
  specified by the worklist module*)
  val enqueue : contents -> t -> t

  (** [exists f [c1; ...; cn]] checks if at least one content of the worklist 
  satisfies the predicate f. That is, it returns (f c1) || (f c2) || ... 
  || (f cn) for a non-empty worklist and false if the worklist is empty*)
  val exists : (contents -> bool) -> t -> bool

  (** [length wl] returns number of elements in the worklist*)
  val length : t -> int
end

module WorklistDefault : WorklistSig with type contents = ObsTbl.t = struct
  type contents = ObsTbl.t

  (** The worklist is a [ObsTbl.t list], where elements are ordered like a
  double-ended queue: tables enqueued with [enqueue] are added to the back of 
  the list and those added with [enqueue_front] are added to the head of the 
  list.
  Invariants for the worklist for the lstar with blanks algorithm (such as
  no duplicates) are enforced outside of the module*)
  type t = contents list
  let empty = []
  let enqueue_front = List.cons
  let enqueue tbl wl = wl @ [tbl]
  let head wl = if wl = empty 
    then failwith "Panic! worklist empty" else List.hd wl
  let tail = function
      | [] -> failwith "Panic! worklist empty"
      | (h::t) -> t
  let map = List.map

  (** [of_list lst] returns a worklist with tables in the same order as in [lst]*)
  let of_list ci_lst : t = List.split ci_lst |> fst
  let exists = List.exists
  let length = List.length
end

module WorklistPQ : WorklistSig with type contents = ObsTbl.t = struct
  type contents = ObsTbl.t
  module S = Set.Make(struct
    type t = contents * int
    let compare (tbl0, priority0) (tbl1, priority1) =
      let s0 = ObsTbl.upper_row_labels tbl0 in
      let s1 = ObsTbl.upper_row_labels tbl1 in
      let size0 = WordSet.cardinal s0 in
      let size1 = WordSet.cardinal s1 in
      let sim0 = ObsTbl.similarity tbl0 in
      let sim1 = ObsTbl.similarity tbl1 in
      (* s0, s1 needed at end because otherwise the equivalence is too coarse;
         different tables would compare equal and some tables will be dropped *)
      if CliOpt.unsat_cores_on () then
        Stdlib.compare (priority0, size0, sim0, s0) (priority1, size1, sim1, s1)
      else if CliOpt.uniq_on () then
        Stdlib.compare (size0, priority0, sim0, s0) (size1, priority1, sim1, s1)
      else
        Stdlib.compare (size0, s0) (size1, s1)
  end)

  (** The worklist is a [(ObsTbl.t * int) Set.S] whose elements are ordered by an
  [S.compare] function.*)
  type t = S.t
  let empty = S.empty
  let enqueue_front tbl = S.add (tbl, 0)
  let head wl = S.min_elt wl |> fst
  let tail wl = S.remove (S.min_elt wl) wl

  (** [enqueue tbl wl] returns [wl] with [tbl] inserted based on the ordering of 
  [S.compare].*)
  let enqueue tbl = S.add (tbl, 1)
  let exists f = S.exists (fun (tbl, pri) -> f tbl)

  let length = S.cardinal
end
