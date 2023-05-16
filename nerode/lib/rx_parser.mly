%token LPAR RPAR EOF
%token PLUS AND DOT STAR QMARK NEG
%token E X
%token <int> NUM

%start <Rx.t> rx_eof

%%

rx_eof:
  | r=rx; EOF { r }
  ;

rx:
  | r=prx { r }

prx:
  | r1=irx; PLUS; r2=prx { Rx.union_pair r1 r2 }
  | r=irx { r }

irx:
  | r1=drx; AND; r2=irx { Rx.intersect_pair r1 r2 }
  | r=drx { r }

drx:
  | r1=urx; DOT; r2=drx { Rx.seq_pair r1 r2 }
  | c1=cx; r2=drx { Rx.seq_pair c1 r2 }
  | r=urx { r }

urx:
  | r=urx; QMARK { Rx.qmark r }
  | r=urx; STAR { Rx.star r }
  | r=urx; NEG { Rx.neg r }
  | r=arx { r }

arx:
  | c=cx { c }
  | LPAR; r=rx; RPAR { r }
  ;

cx:
  | n = NUM { Rx.Char (Alphabet.sym_of_int n) }
  | X { Rx.(union_pair (Char (Alphabet.sym_of_int 0)) (Char (Alphabet.sym_of_int 1))) }
  | E { Rx.Epsilon }
  ;

%%
