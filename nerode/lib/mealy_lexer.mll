%option noyywrap (*remove this?*)
{
open Mealy_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}


let whitespace = '\t' | ' '
let newline = "\r\n" | '\r' | '\n'
let poundline = '#' [ ^newline ]* (*trying to do everything from pound sign until a new line*)
let in_out = ['0' '1' '-']+ 
let id = ['0'-'9' 'A'-'Z' 'a'-'z' '_' '-' '.']+

rule next_token = parse
  | whitespace+ { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf (*look to next line *)}
  | poundline   { next_token lexbuf } (*check if this is right*)
  | ".i"		{ INPUTS }
  | ".o"		{ OUTPUTS }
  | ".p"		{ TRANSITIONS }
  | ".s"		{ STATES }
  | ".r"		{ RESETSTATE } (*start state*)
  | ".e"		{ END } (*need to make optional? maybe not if we have EOF as well*)
  | eof     {EOF} (*is this the same as END?*)
  (* | in_out {IO } *)
  | id      {ID Lexing.lexeme lexbuf} (*these will need to be comparable at some point*)

IDCHAR [0-9A-Za-z_\-\.] (*how to rewrite this?*)

%%

(* #[^\n]*\n	{ yylineno++; /* Ignore comments starting with pound signs */ }

\n		{ yylineno++; }

{WHITESPACE}+	{ /* Ignore whitespace */ }

".i"		{ INPUTS }
".o"		{ OUTPUTS }
".p"		{ TRANSITIONS; }
".s"		{ STATES }
".r"		{ RESETSTATE } (*start state*)
".e"		{ END } (*need to make optional*)

{IDCHAR}+	{ yylval.s = new char[yyleng+1];
		  strcpy(yylval.s, yytext);
		  return ID; }
%% *)
