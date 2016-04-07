exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = IntS of int
             | FracS of exprS * exprS
             | NumS of float
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

(* You will need to add more cases here. *)
type exprC = IntC of int
             | FracC of exprC * exprC
             | NumC of float
             | PinfC
             | NinfC
             | NanC 
             | BoolC of bool
             | IfC of exprC * exprC * exprC
             | ArithC of string * exprC * exprC
             | CompC of string * exprC * exprC
             | EqC of exprC * exprC


(* You will need to add more cases here. *)
type value = Int of int
             | Frac of value * value
             | Num of float
             | Pinf 
             | Ninf 
             | Nan 
             | Bool of bool

type 'a env = (string * 'a) list
let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)
let rec arithEval op v1 v2 = match (op, v1, v2) with
                         | (_, Num x, Int y) -> arithEval op v1 (Num (float_of_int y))
                         | (_, Int x, Num y) -> arithEval op (Num (float_of_int x)) v2
                         | ("/", Num x, Num 0.0) -> if (x = 0.0) then (Nan "+nan.0") 
                                                    else if (x > 0.0) then (Pinf max_float) 
                                                         else (Ninf (-. max_float))
                         | ("+", Num x, Num y) -> Num (x +. y) 
                         | ("-", Num x, Num y) -> Num (x -. y)
                         | ("*", Num x, Num y) -> Num (x *. y)
                         | ("/", Num x, Num y) -> Num (x /. y)
                         | (_, Num x, Num y) -> raise (Interp "interpErr: only +, -, *")
                         | ("/", Int x, Int 0) -> if (x = 0) then (Nan "+nan.0") 
                                                  else if (x > 0) then (Pinf max_float) 
                                                       else Ninf (-. max_float)
                         | ("+", Int x, Int y) -> Int (x + y) 
                         | ("-", Int x, Int y) -> Int (x - y)
                         | ("*", Int x, Int y) -> Int (x * y)
                         | ("/", Int x, Int y) -> Int (x / y)
                         | (_, Int x, Int y) -> raise (Interp "interpErr: only +, -, *")
                         | _ -> raise (Interp "interpErr: not a num")


let compEval op v1 v2 = match (op, v1, v2) with
                        | (">", Num x, Num y)   -> Bool (x > y) 
                        | (">=", Num x, Num y)  -> Bool (x >= y)
                        | ("<", Num x, Num y)   -> Bool (x < y)
                        | ("<=", Num x, Num y)  -> Bool (x <= y)
                        | (_, Num x, Num y)     -> raise (Interp "interpErr: only <, <=, >, >=")
                        | _                     -> raise (Interp "interpErr: not a num")


let eqEval v1 v2 = match (v1, v2) with
                   | (Num n1, Num n2) -> Bool (n1 = n2)
                   | (Bool b1, Bool b2) -> Bool (b1 = b2)
                   | _ -> Bool false
(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | IntS i        -> IntC i
  | NumS          -> NumC i
  | PinfS         -> PinfC 
  | NinfS         -> NinfC 
  | NanS          -> NanC 
  | BoolS i       -> BoolC i
  | FracS (v1, v2)      -> FracC (desugar v1, desugar v2)
  | IfS (cond, th, els) -> IfC (desugar cond, desugar th, desugar els)
  | NotS e -> desugar (IfS (e, BoolS false, BoolS true))
  | OrS (e1, e2) -> desugar (IfS (e1, BoolS true, IfS (e2, BoolS true, BoolS false)))
  | AndS (e1, e2) -> desugar (IfS (e1, IfS(e2, BoolS true, BoolS false), BoolS false))
  | ArithS (op, v1, v2) -> ArithC (op, desugar v1, desugar v2)
  | CompS (op, v1, v2) -> CompC (op, desugar v1, desugar v2)
  | EqS (v1, v2) -> EqC (desugar v1, desugar v2)
  | NeqS (v1, v2) -> desugar (NotS (EqS (v1, v2)))


(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | IntC i        -> Int i
  | NumC i        -> Num i
  | PinfC         -> Pinf
  | NinfC         -> Ninf 
  | NanC          -> Nan 
  | BoolC i       -> Bool i
  | FracC (v1, v2)   -> ( match (interp env v1, interp env v2) with
                          | (Int i1, Int i2) -> if (i2 = 0) then raise (Interp "interpErr: zero denumerator")
                                                else if (i1 = 0) then Int 0
                                                     else Frac (Int i1, Int i2)
                          | _ -> raise (Interp "interpErr: not an int"))
  | IfC (i1, i2, i3) -> ( match (interp env i1) with
                          | Bool i1' -> if (i1') then interp env i2 
                                                 else interp env i3
                          | _ -> raise (Interp "interpErr: only boolean") )
  | ArithC (op, v1, v2) -> arithEval op (interp env v1) (interp env v2)
  | CompC (op, v1, v2) -> compEval op (interp env v1) (interp env v2)
  | EqC (v1, v2) -> eqEval (interp env v1) (interp env v2)


(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []



(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Int i                   -> string_of_int i
  | Frac (Int i1, Int i2)   -> (string_of_int i1) ^ "/" ^ (string_of_int i2)
  | Num i                   -> string_of_float i
  | Pinf                    -> "+inf.0"
  | Ninf                    -> "-inf.0"
  | Nan                     -> "+nan.0"
  | Bool i                  -> string_of_bool i
