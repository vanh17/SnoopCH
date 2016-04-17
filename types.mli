exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

type 'a env

type exprS = IntS of int
             | FracS of (int * int)
             | NumS of float
             | ComplexFrS of (int * int * int * int)
             | ComplexFnS of (int * int * float)
             | ComplexNfS of (float * int * int)
             | ComplexNS of (float * float)
             | PinfS
             | NinfS
             | NanS
             | BoolS of bool
             | StringS of string
             | CharS of char
             | CharNullS of string
             | EmptyS
             | IfS of exprS * exprS * exprS
             | OrS of exprS * exprS
             | AndS of exprS * exprS
             | NotS of exprS
             | ArithS of string * exprS * exprS
             | CompS of string * exprS * exprS
             | EqS of exprS * exprS
             | NeqS of exprS * exprS
             | CondS of (exprS * exprS) list
             | ListS of exprS list
             | PairS of exprS * exprS
             | CarS of exprS
             | CdrS of exprS
             | NullS
             | VarS of string
             | LetS of ((exprS * exprS) list) * exprS
             | LetsS of ((exprS * exprS) list) * exprS
             | LetrS of ((exprS * exprS) list) * exprS
             | FunS of ((exprS list) * exprS)
             | IsStringS of exprS
             | IsCharS of exprS
             | CharToIntS of exprS
             | IntToCharS of exprS
             | MakeStringS of exprS * exprS
             | DefineS of exprS * exprS
             | CallS of (exprS * (exprS list))
type exprC = IntC of int
             | FracC of (int * int)
             | NumC of float
             | ComplexFrC of (int * int * int * int)
             | ComplexFnC of (int * int * float)
             | ComplexNfC of (float * int * int)
             | ComplexNC of (float * float)
             | PinfC
             | NinfC
             | NanC 
             | BoolC of bool
             | StringC of string
             | CharC of char
             | CharNullC of string
             | EmptyC
             | IfC of exprC * exprC * exprC
             | ArithC of string * exprC * exprC
             | CompC of string * exprC * exprC
             | EqC of exprC * exprC
             | CondC of (exprC * exprC) list
             | ListC of exprC list
             | PairC of exprC * exprC
             | CarC of exprC
             | CdrC of exprC
             | NullC
             | VarC of string
             | LetC of ((exprC * exprC) list) * exprC
             | LetsC of ((exprC * exprC) list) * exprC
             | LetrC of ((exprC * exprC) list) * exprC
             | FunC of ((exprC list) * exprC)
             | IsStringC of exprC
             | IsCharC of exprC
             | CharToIntC of exprC
             | IntToCharC of exprC
             | MakeStringC of exprC * exprC
             | DefineC of exprC * exprC
             | CallC of (exprC * (exprC list))
type value = Int of int
             | Frac of (int * int)
             | Num of float
             | ComplexFr of (int * int * int * int)
             | ComplexFn of (int * int * float)
             | ComplexNf of (float * int * int)
             | ComplexN of (float * float)
             | Pinf
             | Ninf
             | Nan 
             | Bool of bool
             | String of string
             | Chara of char
             | CharNull of string
             | Empty
             | List of value list
             | Pair of value * value
             | Null
             | FunClos of (exprC * (value env))
             | RefToOpV of (value option) ref


(* Environment lookup *)
val empty : 'a env
val lookup : 'a -> ('a * value) list -> value option (*string -> 'a env -> 'a option*)
val bind :  string -> 'a -> 'a env -> 'a env
val bindList : exprC list -> 'a list -> 'a env -> 'a env

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : value env -> exprC -> value
val evaluate : exprC -> value

(* result post-processing *)
val valToString : value -> string

(* helper methods *)
val toNum : value -> value
val gcm : int -> int -> int
val isPos : value -> bool
val simplify_frac : value -> value
val simplify_complex : value -> value
val conjugate : value -> value
val toComplexN : value -> value
val isComplex : value -> bool
val arithEval : string -> value -> value -> value
val compEval : string -> value -> value -> value
val eqEval : value -> value -> value
val condEval : (exprC * exprC) list -> exprC
val isPair : value -> bool
val isList : value -> bool
val isNum : value -> bool
