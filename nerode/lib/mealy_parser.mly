// %{ // -*- C++ -*-

//      /* yacc grammar for KISS files */

//      /* Stephen Edwards */

// #include "utils.hh"
// #include "string.hh"
// #include "array.hh"
// #include "mmm.hh"

// extern int yylex(void);	// from scan.cc
// void yyerror(char * s);

// extern FILE *yyin;	// from gram.tab.cc
// extern int yylineno;	// from scan.cc

// extern fsm *machine;

// %}

// %union {
//   char *s;
// }

{
  let empty_decs : Mealy.parseddec = {
    in_len = None
    out_len = None
    ntrans = None
    nstates = None
    start = None
  }
}

%token INPUTS OUTPUTS TRANSITIONS STATES RESETSTATE END
%token EOF
%token IO

%token <string> ID

%start <Mealy.parsed> file (*file gives start point*)

%%

file :
    decs=declarations; trans=transitions; end {(decs, trans)}
  ;

(*
*)
declarations : 
  | declarations1 | { (* Empty *) }
  ;

declarations1 :
  | decs=declarations1; INPUTS in_len=ID 
    {{decs with in_len= Some (int_of_string in_len)}}
  | decs=declarations1; OUTPUTS out_len=ID 
    {{decs with out_len= Some (int_of_string out_len)}}
  | decs=declarations1; TRANSITIONS n_trans=ID 
    {{decs with ntrans= Some (int_of_string ntrans)}}
  | decs=declarations1; STATES n_states=ID 
    {{decs with nstates= Some (int_of_string nstates)}}
  | decs=declarations1; RESETSTATE startst=ID 
    {{decs with start= Some start}}
  | INPUTS in_len=ID	{ {empty_decs with in_len= Some (int_of_string in_len)} }
  | OUTPUTS out_len=ID	{ {empty_decs with out_len= Some (int_of_string out_len)} }
  | TRANSITIONS ntrans=ID { {empty_decs with ntrans= Some (int_of_string ntrans)} }
  | STATES nstates=ID	{ {empty_decs with nstates= Some (int_of_string nstates)} }
  | RESETSTATE start=ID  { {empty_decs with start= Some start} }
  ;

transitions :
  | translst=transitions; trans=transition {trans::translst}
  | trans=transition {[trans]}
  ;

transition:
  | inp=ID; s1=ID; s2=ID; outp=ID	{ ((inp, s1), (s2, outp)) }
  // | inp=IO; s1=IO; s2=ID; outp=IO	{ ((inp, s1), (s2, outp)) }
  // | inp=IO; s1=ID; s2=IO; outp=IO	{ ((inp, s1), (s2, outp)) }
  // | inp=IO; s1=IO; s2=IO; outp=IO	{ ((inp, s1), (s2, outp)) }
  ;

end :
  | END
  |	EOF
  ;

%%

// void
// yyerror(char * s)
// {
//   cerr << s << " at line " << yylineno << '\n';
// }