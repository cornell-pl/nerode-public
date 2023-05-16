(* Adapted from grammar for Oliveira's BICA *)

{
open Rx_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'

let bitvec = ['0'-'1']

(*
Oliveira's scan.l
#[^\n]*\n	{ yylineno++; /* Ignore comments starting with pound signs */ }

\n		{ yylineno++; }

{WHITESPACE}+	{ /* Ignore whitespace */ }

".i"		{ return INPUTS; }
".o"		{ return OUTPUTS; }
".p"		{ return TRANSITIONS; }
".s"		{ return STATES; }
".r"		{ return RESETSTATE; }
".e"		{ return END; }

IDCHAR [0-9A-Za-z_\-\.]
{IDCHAR}+	{ yylval.s = new char[yyleng+1];
		  strcpy(yylval.s, yytext);
		  return ID; }
*)

rule next_token = parse
  | whitespace+
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }

  | bitvec { BITVEC (int_of_string ("0b" ^ (Lexing.lexeme lexbuf))) }
  | ".i" { INPUTS }
  | ".o" { OUTPUTS }
  | ".p" { TRANSITIONS }
  | ".s" { STATES }
  | ".r" { RESETSTATE }
  | ".e" { END }
  | ['0'-'9' 'A'-'Z' 'a'-'z' '-' '.' ]+ { ID }

  (* EOF/illegal token *)
  | eof { EOF }
  | _ as c { illegal c }
