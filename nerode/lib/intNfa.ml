(** Instance of NFA, where states are type [int] *)

module FInt = struct
  module StateSet = Set.Make(Int)
  include Int
  let fresh (s: StateSet.t) = (StateSet.max_elt s) + 1
end

include Nfa.Make(FInt)


let trans_to_json trans_list = List.map
  (fun (s1, x, s2) -> `List [`Int s1; Alphabet.to_json x; `Int s2]) trans_list

let json_to_trans json = List.map
  (fun j -> match j with
            | `List [`Int s1; x; `Int s2] -> (s1, Alphabet.sym_of_json x, s2)
            | _ -> failwith "Invalid json") json

let of_json json = failwith "unimplemented!!"

let to_json (nfa: t) = failwith "nothing here!!!"
