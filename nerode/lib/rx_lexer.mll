{
open Rx_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'

let num = ['0'-'9']

rule next_token = parse
  | whitespace+
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }

  (* actual tokens here *)
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  (*
  | "0" { ZERO } (* Alphabet? *)
  | "1" { ONE }
  *)
  | 'X' { X }
  | "e" { E }
  | '(' { LPAR }
  | ')' { RPAR }
  | '+' { PLUS }
  | '|' { PLUS }
  | '.' { DOT }
  | ';' { DOT }
  | '*' { STAR }
  | '?' { QMARK}
  | '^' { NEG }
  | '&' { AND }

  (* EOF/illegal token *)
  | eof { EOF }
  | _ as c { illegal c }
