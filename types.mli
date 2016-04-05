exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

type exprS = IntS of int
             | FracS of exprS * exprS
             | NumS of float
             | PinfS of float
             | NinfS of float
             | NanS of string
             | BoolS of bool
             | IfS of exprS * exprS * exprS
             | OrS of exprS * exprS
             | AndS of exprS * exprS
             | NotS of exprS
             | ArithS of string * exprS * exprS
             | CompS of string * exprS * exprS
             | EqS of exprS * exprS
             | NeqS of exprS * exprS
type exprC = IntC of int
             | FracC of exprC * exprC
             | NumC of float
             | PinfC of float
             | NinfC of float
             | NanC of string 
             | BoolC of bool
             | IfC of exprC * exprC * exprC
             | ArithC of string * exprC * exprC
             | CompC of string * exprC * exprC
             | EqC of exprC * exprC
type value = Int of int
             | Frac of value * value
             | Num of float
             | Pinf of float
             | Ninf of float
             | Nan of string
             | Bool of bool


(* Environment lookup *)
type 'a env
val empty : 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : value env -> exprC -> value
val evaluate : exprC -> value

(* result post-processing *)
val valToString : value -> string

(* helper methods *)
val arithEval : string -> value -> value -> value
val compEval : string -> value -> value -> value
val eqEval : value -> value -> value