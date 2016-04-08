exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
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
             | IfC of exprC * exprC * exprC
             | ArithC of string * exprC * exprC
             | CompC of string * exprC * exprC
             | EqC of exprC * exprC


(* You will need to add more cases here. *)
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
let toNum fr = match fr with
               | Frac (x, y) -> Num ((float_of_int x) /. (float_of_int y))
               | _ -> raise (Interp "interpErr: not a fraction")

let rec gcm x y = if y = 0 then x else gcm y (x mod y)

let isPos x = match x with
              | Int i -> i > 0
              | Num i -> i > 0.0
              | Frac (x, y) -> x * y > 0

let simplify_frac fr = match fr with
                       | Frac (_, 0) -> raise (Interp "interpErr: zero denumerator")
                       | Frac (0, _) -> Int 0
                       | Frac (x, y) -> let t = x / y 
                                        in if (t * y = x) then Int t 
                                           else let g = gcm x y 
                                                in  Frac (x / g, y /g)

               
let rec arithEval op v1 v2 = match (op, v1, v2) with
                         | (_, Num x, Int y) -> arithEval op v1 (Num (float_of_int y))
                         | (_, Int x, Num y) -> arithEval op (Num (float_of_int x)) v2
                         | (_, Frac (x1, x2), Int y) -> arithEval op v1 (Frac (y, 1))
                         | (_, Int x, Frac (y1, y2)) -> arithEval op (Frac (x, 1)) v2
                         | (_, Frac (x1, x2), Num y) -> arithEval op (toNum v1) v2
                         | (_, Num x, Frac (y1, y2)) -> arithEval op v1 (toNum v2)
                         | ("/", Num x, Num 0.0) -> if (x = 0.0) then Nan 
                                                    else if (x > 0.0) then Pinf 
                                                         else Ninf
                         | ("+", Num x, Num y) -> Num (x +. y) 
                         | ("-", Num x, Num y) -> Num (x -. y)
                         | ("*", Num x, Num y) -> Num (x *. y)
                         | ("/", Num x, Num y) -> Num (x /. y)
                         | (_, Num x, Num y) -> raise (Interp "interpErr: only +, -, *")
                         | ("/", Int x, Int 0) -> if (x = 0) then Nan 
                                                  else if (x > 0) then Pinf 
                                                       else Ninf
                         | ("+", Int x, Int y) -> Int (x + y) 
                         | ("-", Int x, Int y) -> Int (x - y)
                         | ("*", Int x, Int y) -> Int (x * y)
                         | ("/", Int x, Int y) -> Int (x / y)
                         | (_, Int x, Int y) -> raise (Interp "interpErr: only +, -, *")
                         | ("+", Frac (x1, x2), Frac (y1, y2)) -> simplify_frac (Frac (x1 * y2 + y1 * x2, x2 * y2)) 
                         | ("-", Frac (x1, x2), Frac (y1, y2)) -> simplify_frac (Frac (x1 * y2 - y1 * x2, x2 * y2))
                         | ("*", Frac (x1, x2), Frac (y1, y2)) -> simplify_frac (Frac (x1 * y1, x2 * y2))
                         | ("/", Frac (x1, x2), Frac (y1, y2)) -> simplify_frac (Frac (x1 * y2, x2 * y1))
                         | (_, Frac (x1, x2), Frac (y1, y2)) -> raise (Interp "interpErr: only +, -, *")
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
  | NumS i          -> NumC i
  | ComplexFrS i -> ComplexFrC i
  | ComplexFnS i -> ComplexFnC i
  | ComplexNfS i -> ComplexNfC i
  | ComplexNS i  -> ComplexNC i
  | PinfS         -> PinfC 
  | NinfS         -> NinfC 
  | NanS          -> NanC 
  | BoolS i       -> BoolC i
  | FracS (v1, v2)      -> FracC (v1, v2)
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
  | ComplexFrC i  -> ComplexFr i
  | ComplexFnC i  -> ComplexFn i
  | ComplexNfC i  -> ComplexNf i
  | ComplexNC i   -> ComplexN i
  | PinfC         -> Pinf
  | NinfC         -> Ninf 
  | NanC          -> Nan 
  | BoolC i       -> Bool i
  | FracC (v1, v2)   -> simplify_frac (Frac (v1, v2))
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
  | Frac (i1, i2)           -> (string_of_int i1) ^ "/" ^ (string_of_int i2)
  | Num i                   -> string_of_float i
  | ComplexFn (i1, i2, i3)  -> (valToString (evaluate (FracC (i1, i2)))) ^ (if i3 > 0.0 then "+" else "-") ^ (string_of_float i3) ^ "i"
  | ComplexNf (i1, i2, i3)  -> (string_of_float i1) ^ (if i2 * i3 > 0 then "+" else "") ^ (valToString (evaluate (FracC (i2, i3)))) ^ "i"
  | ComplexFr (i1, i2, i3, i4) -> (valToString (evaluate (FracC (i1, i2)))) ^ (if i3 * i4 > 0 then "+" else "") ^ (valToString (evaluate (FracC (i3, i4)))) ^ "i"
  | ComplexN (i1, i2)       -> (string_of_float i1) ^ (if i2 > 0.0 then "+" else "") ^ (string_of_float i2) ^ "i"
  | Pinf                    -> "+inf.0"
  | Ninf                    -> "-inf.0"
  | Nan                     -> "+nan.0"
  | Bool i                  -> string_of_bool i
