exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type 'a env = (string * 'a) list

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
             | StringFromLstS of exprS list
             | DefineS of exprS * exprS
             | CallS of (exprS * (exprS list))

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
             | StringFromLstC of exprC list
             | DefineC of exprC * exprC
             | CallC of (exprC * (exprC list))


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
             | String of string
             | Chara of char
             | CharNull of string
             | Empty
             | List of value list
             | Pair of value * value
             | Null
             | FunClos of (exprC * (value env))
             | RefToOpV of (value option) ref

let empty = []
let enref = ref empty

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
                         | []          -> None
                         | (s,v) :: tl -> (if s = str then ( match v with
                                                             | RefToOpV r -> !r
                                                             | _          -> Some v) 
                                          else lookup str tl)
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
              | _ -> raise (Interp "interpErr: not a real number")

let simplify_frac fr = match fr with
                       | Frac (_, 0) -> raise (Interp "interpErr: zero denumerator")
                       | Frac (0, _) -> Int 0
                       | Frac (x, y) -> let t = x / y 
                                        in if (t * y = x) then Int t 
                                           else let g = gcm x y 
                                                in  Frac (x / g, y /g)
                       | _ -> raise (Interp "interpErr: not a fraction")


let simplify_complex c = match c with 
                         | ComplexFn (i1, i2, i3) -> let t = simplify_frac (Frac (i1, i2))
                                                     in if (i3 = 0.0) then t else let v =  gcm i1 i2
                                                                                  in ComplexFn (i1/v, i2/v, i3)
                         | ComplexN (i1, i2) -> if i2 = 0.0 then Num i1 else ComplexN (i1, i2)
                         | ComplexNf (i1, i2, i3) -> let t = simplify_frac (Frac (i2, i3))
                                                     in if (t = Int 0) then Num i1 else let v =  gcm i2 i3
                                                                                        in ComplexNf (i1, i2/v, i3/v)
                         | ComplexFr (i1, i2, i3 , i4) -> let (f1, f2) = (simplify_frac (Frac (i1, i2)), simplify_frac (Frac (i3, i4)))
                                                          in if f2 = Int 0 then f1 else let (g1, g2) = (gcm i1 i2, gcm i3 i4)
                                                                                        in ComplexFr (i1/g1, i2/g1, i3/g2, i4/g2)
                         | _ -> raise (Interp "interpErr: not a complex number")

let conjugate c = match c with
                  | ComplexFn (i1, i2, i3) -> simplify_complex (ComplexFn (i1, i2, i3 *. (-.1.0)))
                  | ComplexN (i1, i2) -> simplify_complex (ComplexN (i1, (-. 1.0) *. i2))
                  | ComplexFr (i1, i2, i3, i4) -> simplify_complex (ComplexFr (i1, i2, -i3, i4))
                  | ComplexNf (i1, i2, i3) -> simplify_complex (ComplexNf (i1, -i2, i3))
                  | Int i -> c
                  | Num i -> c
                  | Frac i -> c
                  | _ -> raise (Interp "interpErr: not a num")


let toComplexN c = match c with
                   | ComplexFr (i1, i2, i3, i4) -> ComplexN ((float_of_int i1)/.(float_of_int i2), (float_of_int i3)/.(float_of_int i4))
                   | ComplexNf (i1, i2, i3) -> ComplexN (i1, (float_of_int i2)/.(float_of_int i3))
                   | ComplexFn (i1, i2, i3) -> ComplexN ((float_of_int i1)/.(float_of_int i2), i3)
                   | ComplexN (i1, i2) -> c
                   | Int i1 -> ComplexN (float_of_int i1, 0.0)
                   | Num i1 -> ComplexN (i1, 0.0)
                   | Frac (i1, i2) -> ComplexN ((float_of_int i1)/.(float_of_int i2), 0.0)
                   | _ -> raise (Interp "interpErr: not a num")

let isComplex c = match c with
                  | ComplexN (i1, i2) -> true 
                  | ComplexFn (i1, i2, i3) -> true
                  | ComplexNf (i1, i2, i3) -> true
                  | ComplexFr (i1, i2, i3, i4) -> true
                  | _ -> false

               
let rec arithEval op v1 v2 = match (op, v1, v2) with
                         | ("/", _, Int 0) -> raise (Interp "interpErr: division with 0")
                         | ("/", _, Num 0.0) -> raise (Interp "interpErr: division with 0")
                         | (_, ComplexFn i, _) -> arithEval op (toComplexN v1) (toComplexN v2)
                         | (_, ComplexNf i, _) -> arithEval op (toComplexN v1) (toComplexN v2)
                         | (_, _, ComplexFn i) -> arithEval op (toComplexN v1) (toComplexN v2)
                         | (_, _, ComplexNf i) -> arithEval op (toComplexN v1) (toComplexN v2)
                         | (_, Num x, Int y) -> arithEval op v1 (Num (float_of_int y))
                         | (_, Int x, Num y) -> arithEval op (Num (float_of_int x)) v2
                         | (_, Frac (x1, x2), Int y) -> arithEval op v1 (Frac (y, 1))
                         | (_, Int x, Frac (y1, y2)) -> arithEval op (Frac (x, 1)) v2
                         | (_, Frac (x1, x2), Num y) -> arithEval op (toNum v1) v2
                         | (_, Num x, Frac (y1, y2)) -> arithEval op v1 (toNum v2)
                         | ("+", Num x, Num y) -> Num (x +. y) 
                         | ("-", Num x, Num y) -> Num (x -. y)
                         | ("*", Num x, Num y) -> Num (x *. y)
                         | ("/", Num x, Num y) -> Num (x /. y)
                         | (_, Num x, Num y) -> raise (Interp "interpErr: only +, -, *")
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
                         | ("/", ComplexFr (x1, x2, x3, x4), Int i1) -> arithEval "*" v1 (ComplexFr (1, i1, 0, 1))
                         | ("/", ComplexN (x1, x2), Num i1) -> arithEval "*" v1 (ComplexN (1.0 /. i1, 0.0))
                         | (_, Frac (x1, x2), _) -> arithEval op (ComplexFr (x1, x2, 0, 1)) v2
                         | (_, Int x1, _) -> arithEval op (ComplexFr (x1, 1, 0, 1)) v2
                         | (_, Num x1, _) -> arithEval op (ComplexN (x1, 0.0)) v2
                         | (_, _, Int x1) -> arithEval op v1 (ComplexFr (x1, 1, 0, 1))
                         | (_, _, Num x1) -> arithEval op v1 (ComplexN (x1, 0.0))
                         | (_, _, Frac (x1, x2)) -> arithEval op v1 (ComplexFr (x1, x2, 0, 1))
                         | ("+", ComplexFr (x1, x2, x3, x4), ComplexFr (y1, y2, y3, y4)) -> ( match (arithEval "+" (Frac (x1, x2)) (Frac (y1, y2)), arithEval "+" (Frac (x3, x4)) (Frac (y3, y4))) with
                                                                                              | (f1, Int 0) -> f1
                                                                                              | (Int i1, Int i2) -> ComplexFr (i1, 1, i2, 1)
                                                                                              | (Int i1, Frac (i2, i3)) -> ComplexFr (i1, 1, i2, i3)
                                                                                              | (Frac (i1, i2), Int i3) -> ComplexFr (i1, i2, i3, 1)
                                                                                              | (Frac (i1, i2), Frac (i3, i4)) -> ComplexFr (i1, i2, i3, i4) 
                                                                                              | _ -> raise (Interp "interpErr: arithmetic operation on complex number") )
                         | ("-", ComplexFr (x1, x2, x3, x4), ComplexFr (y1, y2, y3, y4)) -> ( match (arithEval "-" (Frac (x1, x2)) (Frac (y1, y2)), arithEval "-" (Frac (x3, x4)) (Frac (y3, y4))) with
                                                                                              | (f1, Int 0) -> f1
                                                                                              | (Int i1, Int i2) -> ComplexFr (i1, 1, i2, 1)
                                                                                              | (Int i1, Frac (i2, i3)) -> ComplexFr (i1, 1, i2, i3)
                                                                                              | (Frac (i1, i2), Int i3) -> ComplexFr (i1, i2, i3, 1)
                                                                                              | (Frac (i1, i2), Frac (i3, i4)) -> ComplexFr (i1, i2, i3, i4) 
                                                                                              | _ -> raise (Interp "interpErr: arithmetic operation on complex number") )
                         | ("*", ComplexFr (x1, x2, x3, x4), ComplexFr (y1, y2, y3, y4)) -> ( match (arithEval "-" (arithEval "*" (Frac (x1, x2)) (Frac (y1, y2))) (arithEval "*" (Frac (x3, x4)) (Frac (y3, y4))), arithEval "+" (arithEval "*" (Frac (x1, x2)) (Frac (y3, y4))) (arithEval "*" (Frac (x3, x4)) (Frac (y1, y2)))) with
                                                                                              | (f1, Int 0) -> f1
                                                                                              | (Int i1, Int i2) -> ComplexFr (i1, 1, i2, 1)
                                                                                              | (Int i1, Frac (i2, i3)) -> ComplexFr (i1, 1, i2, i3)
                                                                                              | (Frac (i1, i2), Int i3) -> ComplexFr (i1, i2, i3, 1)
                                                                                              | (Frac (i1, i2), Frac (i3, i4)) -> ComplexFr (i1, i2, i3, i4) 
                                                                                              | _ -> raise (Interp "interpErr: arithmetic operation on complex number") )
                         | ("/", ComplexFr (x1, x2, x3, x4), ComplexFr (y1, y2, y3, y4)) -> arithEval "*" v1 (arithEval "/" (conjugate v2) (arithEval "*" v2 (conjugate v2)))
                         | (_, ComplexFr (x1, x2, x3, x4), ComplexFr (y1, y2, y3, y4)) -> raise (Interp "interpErr: only +, -, *")
                         | ("+", ComplexN (x1, x2), ComplexN (y1, y2)) -> ( match (arithEval "+" (Num x1) (Num y1), arithEval "+" (Num x2) (Num y2)) with
                                                                            | (f1, Num 0.0) -> f1
                                                                            | (Num i1, Num i2) -> ComplexN (i1, i2) 
                                                                            | _ -> raise (Interp "interpErr: arithmetic operation on complex number") )
                         | ("-", ComplexN (x1, x2), ComplexN (y1, y2)) -> ( match (arithEval "-" (Num x1) (Num y1), arithEval "-" (Num x2) (Num y2)) with
                                                                            | (f1, Num 0.0) -> f1
                                                                            | (Num i1, Num i2) -> ComplexN (i1, i2) 
                                                                            | _ -> raise (Interp "interpErr: arithmetic operation on complex number") )
                         | ("*", ComplexN (x1, x2), ComplexN (y1, y2)) -> ( match (arithEval "-" (arithEval "*" (Num x1) (Num y1)) (arithEval "*" (Num x2) (Num y2)), arithEval "+" (arithEval "*" (Num x1) (Num y2)) (arithEval "*" (Num x2) (Num y1))) with
                                                                            | (f1, Num 0.0) -> f1
                                                                            | (Num i1, Num i2) -> ComplexN (i1, i2) 
                                                                            | _ -> raise (Interp "interpErr: arithmetic operation on complex number") )
                         | ("/", ComplexN (x1, x2), ComplexN (y1, y2)) -> arithEval "*" v1 (arithEval "/" (conjugate v2) (arithEval "*" v2 (conjugate v2)))
                         | (_, ComplexN (x1, x2), ComplexN (y1, y2)) -> raise (Interp "interpErr: only +, -, *")
                         | _ -> raise (Interp "interpErr: not a num")


let compEval op v1 v2 = match (op, v1, v2) with
                        | (">", Num x, Num y)   -> Bool (x > y) 
                        | (">=", Num x, Num y)  -> Bool (x >= y)
                        | ("<", Num x, Num y)   -> Bool (x < y)
                        | ("<=", Num x, Num y)  -> Bool (x <= y)
                        | (_, Num x, Num y)     -> raise (Interp "interpErr: only <, <=, >, >=")
                        | _                     -> raise (Interp "interpErr: not a num")

let isNum n = match n with
              | Num _ -> true
              | Frac _ -> true
              | ComplexN _ -> true
              | ComplexNf _ -> true
              | ComplexFr _ -> true
              | ComplexFn _ -> true
              | Int _ -> true
              | _ -> false

let eqEval v1 v2 = match (v1, v2) with
                   | (Bool b1, Bool b2) -> Bool (b1 = b2)
                   | _ -> if ((isNum v1) && (isNum v2)) then ( match (toComplexN v1, toComplexN v2) with
                                                               | (ComplexN (x1, x2), ComplexN (y1, y2)) -> Bool (x1 = y1 && x2 = y2)
                                                               | _ -> Bool false )
                          else Bool false

let rec condEval lst = match lst with
                       | [] -> EmptyC
                       | (BoolC true, r) :: rest -> r
                       | (BoolC false, _) :: rest -> condEval rest
                       | (_, r) :: rest -> r

let isPair e = match e with
               | Pair (_, _) -> true
               | _ -> false

let isList e = match e with
               | List i -> true
               | _ -> false 

let rec bindList lstVar lstVal env = match (lstVar, lstVal) with
                                     | ([], []) -> env
                                     | ((VarC var1) :: varest, val1 :: vlrest) -> let e = bind var1 val1 env 
                                                                           in bindList varest vlrest e
                                     | _ -> raise (Interp "callErr: arguments and inputs do not match")


(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | IntS i              -> IntC i
  | NumS i              -> NumC i
  | ComplexFrS i        -> ComplexFrC i
  | ComplexFnS i        -> ComplexFnC i
  | ComplexNfS i        -> ComplexNfC i
  | ComplexNS i         -> ComplexNC i
  | PinfS               -> PinfC 
  | NinfS               -> NinfC 
  | NanS                -> NanC 
  | BoolS i             -> BoolC i
  | StringS i           -> StringC i
  | CharS i             -> CharC i
  | CharNullS i         -> CharNullC i
  | EmptyS              -> EmptyC
  | NullS               -> NullC
  | FracS (v1, v2)      -> FracC (v1, v2)
  | IfS (cond, th, els) -> IfC (desugar cond, desugar th, desugar els)
  | NotS e              -> desugar (IfS (e, BoolS false, BoolS true))
  | OrS (e1, e2)        -> desugar (IfS (e1, BoolS true, IfS (e2, BoolS true, BoolS false)))
  | AndS (e1, e2)       -> desugar (IfS (e1, IfS(e2, BoolS true, BoolS false), BoolS false))
  | ArithS (op, v1, v2) -> ArithC (op, desugar v1, desugar v2)
  | CompS (op, v1, v2)  -> CompC (op, desugar v1, desugar v2)
  | EqS (v1, v2)        -> EqC (desugar v1, desugar v2)
  | NeqS (v1, v2)       -> desugar (NotS (EqS (v1, v2)))
  | CondS i             -> CondC (List.map (fun (x, y) -> (desugar x, desugar y)) i) 
  | ListS i             -> ListC (List.map (fun (x) -> desugar x) i) 
  | PairS (x, y)        -> PairC (desugar x, desugar y)
  | CarS i              -> CarC (desugar i)
  | CdrS i              -> CdrC (desugar i)
  | VarS i              -> VarC i
  | LetS (lstve, e2)    -> LetC (List.map (fun (v, e) -> (desugar v, desugar e)) lstve, desugar e2)
  | LetsS (lst, e2)     -> LetsC (List.map (fun (x, y) -> (desugar x, desugar y)) lst, desugar e2)
  | LetrS (lst, e2)     -> LetrC (List.map (fun (x, y) -> (desugar x, desugar y)) lst, desugar e2)
  | FunS (arg, b)       -> FunC (List.map (fun x -> desugar x) arg, desugar b)
  | IsStringS i         -> IsStringC (desugar i)
  | IsCharS i           -> IsCharC (desugar i)
  | CharToIntS i        -> CharToIntC (desugar i)
  | IntToCharS i        -> IntToCharC (desugar i)
  | MakeStringS (k, c)  -> MakeStringC (desugar k, desugar c)
  | StringFromLstS clst -> StringFromLstC (List.map (fun x -> desugar x) clst)
  | DefineS (var, value)-> DefineC (desugar var, desugar value)
  | CallS (f, i)        -> CallC (desugar f, List.map (fun x -> desugar x) i)


(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | IntC i              -> Int i
  | NumC i              -> Num i
  | ComplexFrC i        -> simplify_complex (ComplexFr i)
  | ComplexFnC i        -> simplify_complex (ComplexFn i)
  | ComplexNfC i        -> simplify_complex (ComplexNf i)
  | ComplexNC i         -> simplify_complex (ComplexN i)
  | NullC               -> Null
  | PinfC               -> Pinf
  | NinfC               -> Ninf 
  | NanC                -> Nan 
  | BoolC i             -> Bool i
  | StringC i           -> String i 
  | CharC i             -> Chara i 
  | CharNullC i         -> CharNull i   
  | EmptyC              -> Empty
  | FracC (v1, v2)      -> simplify_frac (Frac (v1, v2))
  | IfC (i1, i2, i3)    -> ( match (interp env i1) with
                             | Bool i1' -> if (i1') then interp env i2 
                                           else interp env i3
                             | _ -> raise (Interp "interpErr: only boolean") )
  | ArithC (op, v1, v2) -> arithEval op (interp env v1) (interp env v2)
  | CompC (op, v1, v2)  -> compEval op (interp env v1) (interp env v2)
  | EqC (v1, v2)        -> eqEval (interp env v1) (interp env v2)
  | CondC i             -> if i = [] then raise (Interp "interpErr: cond constructor needs at least one conditon")
                           else interp env (condEval i)
  | ListC i             -> List (List.map (fun (x) -> interp env x) i)
  | PairC (e1, e2)      -> let (v1, v2) = (interp env e1, interp env e2) 
                           in ( match v2 with
                                | List l2 -> List (v1 :: l2)
                                | _ -> Pair (v1, v2) )
  | CarC i              -> ( match (interp env i) with
                             | Pair (v1, v2) -> v1
                             | List (v1 :: rest) -> v1
                             | _ -> raise (Interp "car: expected pair? or list?") ) 
  | CdrC i              -> ( match (interp env i) with
                             | Pair (v1, v2) -> v2
                             | List (v1 :: rest) -> List rest
                             | _ -> raise (Interp "cdr: expected pair? or list?") )
  | VarC i              -> ( match (lookup i env) with
                             | Some v -> v
                             | None   -> raise (Interp "interpErr: undefined variable") )
  | LetC (lstve, e2)    -> let rec bindLet (lst, env1) = ( match lst with
                                                           | []                  -> env1
                                                           | (VarC v, e) :: rest -> bindLet (rest, bind v (interp env e) env1)
                                                           | _ -> raise (Interp "letErr: need at least one binding") )
                           in interp (bindLet (lstve, env)) e2
  | LetsC (lst, e2)     -> let rec bindListPair lst1 env1 = ( match lst1 with
                                                              | [] -> env1
                                                              | (VarC s, v) :: rest -> bindListPair rest (bind s (interp env1 v) env1) 
                                                              | _ -> raise (Interp "let*Err: need at least one binding") )
                           in interp (bindListPair lst env) e2
  | LetrC (lst, e2)     -> let r = ref env
                           in let lst1 = (List.map (fun (VarC v, e) -> ( let r2 = RefToOpV (ref None)
                                                                         in ( match (r:= (bind v r2 (!r))) with
                                                                              | _ -> (VarC v, r2, e) ) ) ) lst) 
                              in ( match (List.map (fun (VarC v, re, e) -> ( match re with
                                                                             | RefToOpV r3 -> (r3:= (Some (interp env e)))
                                                                             | _ -> raise (Interp "letrErr") ) ) lst1) with
                                   | _ -> interp (!r) e2 )
  | FunC (arg, b)       -> FunClos (r, env)
  | IsStringC i         -> ( match (interp env i) with
                             | String _  -> Bool true
                             | _         -> Bool false )
  | IsCharC i           -> ( match (interp env i) with
                             | Chara _    -> Bool true
                             | CharNull _ -> Bool true
                             | _          -> Bool false )
  | CharToIntC i        -> ( match (interp env i) with
                             | Chara v -> Int (Char.code v)
                             | CharNull _ -> Int 0
                             | _       -> raise (Interp "char->integerErr: need an character") )
  | IntToCharC i        -> ( match (interp env i) with
                             | Int v -> if v = 0 then CharNull "nul"
                                        else Chara (Char.chr v)
                             | _     -> raise (Interp "integer->charErr: need a number") )
  | MakeStringC (k, c)  -> ( match (interp env k, interp env c) with
                             | (Int i, Chara v) -> String (let rec aux n s = if n = 0 then "" else s ^ (aux (n - 1) s)
                                                    in aux i (Char.escaped v))
                             | _                -> raise (Interp "makeStringErr: need an integer and single character") )
  | StringFromLstC clst -> String (List.fold_right (fun x y -> ( match x with 
                                                                 | Chara c -> (Char.escaped c) ^ y
                                                                 | _ -> raise (Interp "stringErr: need a list of single charaters") ) ) (List.map (fun t -> interp env t) clst) "")
  | DefineC (var, value)-> ( match var with
                             | VarC v -> let r = RefToOpV (ref None) 
                                         in ( match (enref:= (bind v r (!enref))) with
                                              | _ -> ( match  r with
                                                       | RefToOpV r1 -> ( match (r1:=(Some (interp env value))) with
                                                                          | _ -> Empty )
                                                       | _ -> Empty ) )
                             | _ -> raise (Interp "defineErr: define on non-variable") ) 
  | CallC (f, i)        -> ( match (interp env f, List.map (fun x -> interp env x) i) with
                             | (FunClos (FunC (arg, b), nenv), ilst) -> interp (bindList arg ilst env) b
                             | _ -> raise (Interp "callErr: call on a non-closure") )



(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp (!enref)



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
  | Empty                   -> ""
  | Bool true               -> "#t"
  | Bool false              -> "#f"
  | String i                -> i
  | Chara i                  -> "#\\" ^ (Char.escaped i)
  | CharNull i              -> "#\\" ^ i
  | Null                    -> "'()"
  | List i                  -> let rec listToString lst = match lst with
                                                          | [] -> ""
                                                          | e :: [] -> (valToString e)
                                                          | e1 :: e2 :: rest -> (valToString e1) ^ " " ^ (listToString (e2 :: rest))
                               in "'(" ^ (listToString i) ^ ")"
  | Pair (x, y)             -> "'(" ^ (valToString x) ^ " . " ^ (valToString y) ^ ")"
  | FunClos _               -> "#<procedure>"
  | _                       -> ""
