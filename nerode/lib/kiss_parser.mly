(* Adapted from grammar for Oliveira's BICA *)

%token INPUTS OUTPUTS TRANSITIONS STATES RESETSTATE END
%token ID

%start <Rx.t> rx_eof

%%

file :
    declarations transitions end
  ;

declarations : declarations1 | { /* Empty */ }
  ;

declarations1 :
    declarations1 declaration
  | declaration
  ;

declaration :
    INPUTS ID	{ machine->ninputs($2); delete [] $2; }
  | OUTPUTS ID	{ machine->noutputs($2); delete [] $2; }
  | TRANSITIONS ID { machine->ntransitions($2); delete [] $2; }
  | STATES ID	{ machine->nstates($2); delete [] $2; }
  | RESETSTATE ID  { machine->resetstate($2); }
  ;

transitions :
    transitions transition
  | transition
  ;

transition:
    ID ID ID ID	{ machine->addtrans($1, $2, $3, $4); }
  ;

end :
    END
  |		{ /* empty */ }
  ;

%%
