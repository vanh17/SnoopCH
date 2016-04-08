exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

type exprS = IntS of int
             | FracS of (int * int)
             | NumS of float
             | ComplexS of (int * int)
             | PinfS
             | NinfS
             | NanS
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
             | FracC of (int * int)
             | NumC of float
             | ComplexC of (int * int)
             | PinfC
             | NinfC
             | NanC 
             | BoolC of bool
             | IfC of exprC * exprC * exprC
             | ArithC of string * exprC * exprC
             | CompC of string * exprC * exprC
             | EqC of exprC * exprC
type value = Int of int
             | Frac of (int * int)
             | Num of float
             | Complex of (int * int)
             | Pinf
             | Ninf
             | Nan 
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
val toNum : value -> value
val gcm : int -> int -> int
val simplify_frac : value -> value
val arithEval : string -> value -> value -> value
val compEval : string -> value -> value -> value
val eqEval : value -> value -> value
