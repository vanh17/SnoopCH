%{
  open Types
%}
%token <float> FLOAT
%token <int> INT
%token <char> FRAC
%token <string> COMPOP
%token PINF
%token NINF
%token NAN
%token TRUE FALSE
%token IF THEN ELSE
%token OR AND NOT
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ
%token DBLSEMI
%nonassoc FLOAT
%nonassoc ELSE
%left OR AND
%nonassoc EQ NEQ
%nonassoc NOT
%nonassoc COMPOP
%left  PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Types.exprS> main
%%

main:
  | headEx DBLSEMI               { $1 }
;

headEx:
  | expr                         { $1 }
;

expr:
  | FLOAT                        { NumS $1 }
  | INT                          { IntS $1 }
  | expr FRAC expr               { FracS ($1, $3) }
  | PINF                         { PinfS }
  | NINF                         { NinfS }
  | NAN                          { NanS }
  | TRUE                         { BoolS true }
  | FALSE                        { BoolS false }
  | IF expr THEN expr ELSE expr  { IfS ($2, $4, $6) }
  | OR expr expr                 { OrS ($2, $3) }
  | AND expr expr                { AndS ($2, $3) }
  | NOT expr                     { NotS $2 }
  | PLUS expr expr               { ArithS ("+", $2, $3) }
  | MINUS expr expr              { ArithS ("-", $2, $3) }
  | TIMES expr expr              { ArithS ("*", $2, $3) }
  | DIVIDE expr expr             { ArithS ("/", $2, $3) }
  | COMPOP expr expr             { CompS ($1, $2, $3) }
  | EQ expr expr                 { EqS ($2, $3) }
  | NEQ expr expr                { NeqS ($2, $3) }
;

