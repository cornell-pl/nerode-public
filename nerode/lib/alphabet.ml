type symbol = int

(** Under this reperesentation, each symbol is an index into the
    array of strings. For example if our alphabet is [| "a" ; "b" |] then
    the list of symbols will be [0; 1]. *)
type t = string array

let intalph (n: int) : t = List.map string_of_int (List.init n Fun.id) |> Array.of_list

let size = Array.length

let of_string_array (a: string array) = a

let compare = Stdlib.compare

let symbols (t:t) = List.init (Array.length t) Fun.id

let iter (f:symbol->unit) (t:t) = List.iter f (symbols t)

let fold (f:'a->symbol->'a) (a:'a) (t:t) = List.fold_left f a (symbols t)

let map (f:symbol->'a) (t:t) = List.map f (symbols t)

let sym_of_sexp = Core.Int.t_of_sexp
let sexp_of_sym = Core.Int.sexp_of_t

let sym_of_json = function
  | `Int i -> i
  | `String s -> int_of_string s
  | _ -> failwith "invalid json format"

let sym_to_json i = `Int i

let of_json = function
  | `List lst -> List.map (fun j -> 
    match j with
    | `String s -> s
    | _ -> failwith "Alphabet symbols must be strings") lst |> Array.of_list
  | _ -> failwith "Alphabet must be a json list"

let to_json (a:t) = 
  let lst = Array.to_list a |> List.map (fun s -> `String s) in
  `List lst

let sym_to_string t x = t.(x)

let to_string t =
  Array.to_list t |> String.concat " "
  
let sym_of_int = Fun.id
let sym_to_int = Fun.id

let w_of_ints (lst: int list) = lst


