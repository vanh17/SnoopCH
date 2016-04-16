%{
  open Types
%}
%token <float> FLOAT
%token <int> INT
%token <int * int> FRAC
%token <int * int * int * int> COMF
%token <int * int * float> COMFN
%token <float * int * int> COMNF
%token <float * float> COMN
%token <string> COMPOP
%token <string> STRING
%token <string> CHAR
%token PINF
%token NINF
%token NAN
%token OPAREN
%token CPAREN
%token OSB
%token CSB
%token COND
%token FUN
%token NULL
%token LIST
%token <string> VAR
%token ISSTRING
%token ISCHAR
%token LET
%token LETS
%token LETREC
%token SQUOTE
%token CAR
%token CDR
%token CONS
%token MAP
%token DEFINE
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
  | OPAREN DEFINE  expr expr CPAREN        { DefineS ($3, $4) }

;

listVar:
  | CPAREN                       { [] }
  | VAR listVar                  { (VarS $1) :: $2 }

listExpr:
  | CPAREN                       { [] }
  | expr listExpr                { $1 :: $2 }
;

listVarValuePair:
  | CPAREN                       { [] }
  | varValuePair listVarValuePair { $1 :: $2 }
;

varValuePair:
  | OSB VAR expr CSB             { (VarS $2, $3) }

listPairExpr: 
  | CPAREN                       { [] }                
  | pairExpr listPairExpr        { $1 :: $2 }
;

pairExpr:
  | OSB expr expr CSB            { ($2, $3) }
;


expr:
  | FLOAT                            { NumS $1 }
  | INT                              { IntS $1 }
  | FRAC                             { FracS $1 }
  | COMF                             { ComplexFrS $1 }
  | COMFN                            { ComplexFnS $1 }
  | COMNF                            { ComplexNfS $1 }
  | COMN                             { ComplexNS $1 }
  | PINF                             { PinfS }
  | NINF                             { NinfS }
  | NAN                              { NanS }
  | TRUE                             { BoolS true }
  | FALSE                            { BoolS false }
  | STRING                           { StringS $1 }
  | CHAR                             { CharS $1 }
  | OPAREN COND  listPairExpr        { CondS $3 }              
  | OPAREN IF expr expr expr CPAREN  { IfS ($3, $4, $5) }
  | OPAREN OR expr expr CPAREN       { OrS ($3, $4) }
  | OPAREN AND expr expr CPAREN      { AndS ($3, $4) }
  | OPAREN NOT expr CPAREN           { NotS $3 }
  | OPAREN PLUS expr expr CPAREN     { ArithS ("+", $3, $4) }
  | OPAREN MINUS expr expr CPAREN    { ArithS ("-", $3, $4) }
  | OPAREN TIMES expr expr CPAREN    { ArithS ("*", $3, $4) }
  | OPAREN DIVIDE expr expr CPAREN   { ArithS ("/", $3, $4) }
  | OPAREN COMPOP expr expr CPAREN   { CompS ($2, $3, $4) }
  | OPAREN EQ expr expr CPAREN       { EqS ($3, $4) }
  | OPAREN NEQ expr expr CPAREN      { NeqS ($3, $4) }
  | NULL                             { NullS }
  | OPAREN LIST listExpr             { ListS $3 }
  | SQUOTE OPAREN listExpr           { ListS $3 }
  | OPAREN CONS expr expr CPAREN     { PairS ($3, $4) }
  | OPAREN CAR expr CPAREN           { CarS $3 }
  | OPAREN CDR expr CPAREN           { CdrS $3 }
  | VAR                              { VarS $1 }
  | OPAREN LET OPAREN listVarValuePair expr CPAREN          { LetS ($4, $5) }
  | OPAREN FUN OPAREN listVar expr CPAREN                   { FunS ($4, $5) } 
  | OPAREN OPAREN FUN OPAREN listVar expr CPAREN listExpr   { CallS (FunS ($5, $6), $8) }
  | OPAREN VAR listExpr                                     { CallS (VarS $2, $3) }
  | OPAREN LETS OPAREN listVarValuePair expr CPAREN         { LetsS ($4, $5) }
  | OPAREN LETREC OPAREN listVarValuePair expr CPAREN       { LetrS ($4, $5) }
  | OPAREN ISSTRING expr CPAREN                             { IsStringS $3 }
  | OPAREN ISCHAR expr CPAREN                               { IsCharS $3 }
;

