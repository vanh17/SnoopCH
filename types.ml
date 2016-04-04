exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float
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
type exprC = NumC of float
             | BoolC of bool
             | IfC of exprC * exprC * exprC
             | ArithC of string * exprC * exprC
             | CompC of string * exprC * exprC
             | EqC of exprC * exprC


(* You will need to add more cases here. *)
type value = Num of float
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
let arithEval op v1 v2 = match (op, v1, v2) with
                         | ("+", Num x, Num y) -> Num (x +. y) 
                         | ("-", Num x, Num y) -> Num (x -. y)
                         | ("*", Num x, Num y) -> Num (x *. y)
                         | ("/", Num x, Num y) -> if (y == 0.0) then raise (Interp "interpErr: can't divide 0.0")
                                                  else Num (x /. y)
                         | (_, Num x, Num y) -> raise (Interp "interpErr: only +, -, *")
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
  | NumS i        -> NumC i
  | BoolS i       -> BoolC i
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
  | NumC i        -> Num i
  | BoolC i       -> Bool i
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
  | Num i           -> string_of_float i
  | Bool i          -> string_of_bool i
