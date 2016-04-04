open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3

(* You can also use interp directly to specify a custom environment. *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (BoolC false) = Bool false

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3

let t0e = evaluate (BoolC true) = Bool true

let t0f = evaluate (BoolC false) = Bool false

let tog = let env1 = bind "x" (Bool true) empty
          in interp env1 (BoolC true) = Bool true

let t0h = desugar (BoolS false) = BoolC false

let t0i = evaluate (desugar (BoolS true)) = Bool true



let t1a = evaluate (IfC (BoolC true, NumC 2.3, NumC 3.2)) = Num 2.3

(* You can also use interp directly to specify a custom environment. *)
let t1b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (IfC (BoolC false, BoolC false, BoolC true)) = Bool true

(* You can also test desugar to make sure it behaves properly. *)
let t1c = desugar (IfS (BoolS false, NumS 2.3, NumS 4.5)) = IfC (BoolC false, NumC 2.3, NumC 4.5)

(* Or you can combine with evaluate to get to the final value. *)
let t1d = evaluate (desugar (IfS (BoolS false, NumS 2.3, NumS 4.5))) = Num 4.5

let t1e = evaluate (desugar (IfS (IfS (BoolS false, BoolS false, BoolS true), BoolS false, BoolS true))) = Bool false

let t1f = desugar (IfS (NumS 2.3, BoolS true, BoolS false)) = IfC (NumC 2.3, BoolC true, BoolC false)

let t1f' = try (evaluate (desugar (IfS (NumS 2.3, BoolS true, BoolS false))); false) with Interp "interpErr: only boolean" -> true
                                                                                          | _ -> false

let t1g = try (evaluate (IfC (NumC 2.3, BoolC true, BoolC false)); false) with Interp "interpErr: only boolean" -> true
                                                                               | _ -> false

let t1h = let env1 = bind "x" (Num 3.1) empty
          in try ((interp env1 (IfC (NumC 3.5, BoolC false, BoolC true))); false) with Interp "interpErr: only boolean" -> true
                                                                                       | _ -> false

let t2a = desugar (NotS (BoolS false)) = IfC (BoolC false, BoolC false, BoolC true)

(* You can also use interp directly to specify a custom environment. *)
let t2b = try (evaluate (desugar (NotS (NumS 2.5))); false) with Interp "interpErr: only boolean" -> true
                                                                 | _ -> false

(* You can also test desugar to make sure it behaves properly. *)
let t2c = desugar (NotS (IfS (BoolS false, BoolS false, BoolS true))) = IfC (IfC (BoolC false, BoolC false, BoolC true), BoolC false, BoolC true)

(* Or you can combine with evaluate to get to the final value. *)
let t2d = evaluate (desugar (NotS (IfS (BoolS false, BoolS false, BoolS true)))) = Bool false

let t3a = desugar (OrS (BoolS false, BoolS true)) = IfC (BoolC false, BoolC true, IfC (BoolC true, BoolC true, BoolC false))

(* You can also use interp directly to specify a custom environment. *)
let t3b = try (evaluate (desugar (OrS (NumS 2.5, BoolS true))); false) with Interp "interpErr: only boolean" -> true
                                                                            | _ -> false

(*
let t3c = try (evaluate (desugar (OrS (BoolS true, NumS 2.5))); false) with Interp "interpErr: only boolean" -> true
                                                                            | _ -> false
*)
(* Or you can combine with evaluate to get to the final value. *)
let t3d = evaluate (desugar (OrS (IfS (BoolS false, BoolS false, BoolS true), IfS (BoolS true, BoolS true, BoolS false)))) = Bool true

let t3e = try (evaluate (desugar (OrS (BoolS false, NumS 2.5))); false) with Interp "interpErr: only boolean" -> true
                                                                             | _ -> false

let t3f = desugar (OrS (NumS 2.5, BoolS true)) = IfC (NumC 2.5, BoolC true, IfC (BoolC true, BoolC true, BoolC false))

let t3g = desugar (OrS (NumS 2.5, IfS (BoolS true, NumS 0.0, BoolS false))) = IfC (NumC 2.5, BoolC true, IfC (IfC (BoolC true, NumC 0.0, BoolC false), BoolC true, BoolC false))

let t4a = desugar (AndS (BoolS false, BoolS true)) = IfC (BoolC false, IfC (BoolC true, BoolC true, BoolC false), BoolC false)

(* You can also use interp directly to specify a custom environment. *)
let t4b = try (evaluate (desugar (AndS (NumS 2.5, BoolS true))); false) with Interp "interpErr: only boolean" -> true
                                                                             | _ -> false

(* Or you can combine with evaluate to get to the final value. *)
let t4c = evaluate (desugar (AndS (IfS (BoolS false, BoolS false, BoolS true), IfS (BoolS true, BoolS true, BoolS false)))) = Bool true

(*
let t4d = try (evaluate (desugar (AndS (BoolS false, NumS 2.5))); false) with Interp "interpErr: only boolean" -> true
                                                                              | _ -> false
*)

let t4f = desugar (AndS (NumS 2.5, BoolS true)) = IfC (NumC 2.5, IfC (BoolC true, BoolC true, BoolC false), BoolC false)

let t4g = desugar (AndS (NumS 2.5, IfS (BoolS true, NumS 0.0, BoolS false))) = IfC (NumC 2.5, IfC (IfC (BoolC true, NumC 0.0, BoolC false), BoolC true, BoolC false), BoolC false)

let t4h = try (evaluate (desugar (AndS (BoolS true, NumS 2.5))); false) with Interp "interpErr: only boolean" -> true
                                                                             | _ -> false

let t5a = evaluate (ArithC ("+", NumC 2.5, NumC 3.6)) = Num 6.1

(* You can also use interp directly to specify a custom environment. *)
let t5b = try (evaluate (ArithC (" ", NumC 2.5, BoolC true)); false) with Interp "interpErr: not a num" -> true
                                                                             | _ -> false

(* Or you can combine with evaluate to get to the final value. *)
let t5c = evaluate (ArithC ("*", NumC 4.2, NumC 2.4)) = Num 10.08

(*
let t4d = try (evaluate (desugar (AndS (BoolS false, NumS 2.5))); false) with Interp "interpErr: only boolean" -> true
                                                                              | _ -> false
*)

let t5f = try (evaluate (ArithC ("-", NumC 2.5, BoolC true)); false) with Interp "interpErr: not a num" -> true
                                                                          | _ -> false

let t5g = evaluate (ArithC ("/", NumC 2.5, NumC 1.0)) = Num 2.5

let t5h = try (evaluate (ArithC ("/", NumC 0.0, NumC 0.00)); false) with Interp "interpErr: can't divide 0.0" -> true
                                                                             | _ -> false

let t5i = try (evaluate (ArithC ("/.", NumC 0.0, NumC 0.00)); false) with Interp "interpErr: only +, -, *" -> true
                                                                             | _ -> false

let t5j = try (evaluate (ArithC ("+ ", NumC 0.0, NumC 0.00)); false) with Interp "interpErr: only +, -, *" -> true
                                                                             | _ -> false

let t5i = try (evaluate (ArithC ("/.", NumC 0.0, ArithC ("+", NumC 0.00, BoolC true))); false) with Interp "interpErr: not a num" -> true
                                                                             | _ -> false

let t5g = evaluate (ArithC ("/", ArithC ("*", NumC 4.2, NumC 2.4), ArithC ("/", ArithC ("*", NumC 4.2, NumC 2.4), ArithC ("*", NumC 4.2, NumC 2.4)))) = Num 10.08



let t6a = evaluate (desugar (ArithS ("+", NumS 2.5, NumS 3.6))) = Num 6.1

(* You can also use interp directly to specify a custom environment. *)
let t6b = try (evaluate (desugar (ArithS (" ", NumS 2.5, BoolS true))); false) with Interp "interpErr: not a num" -> true
                                                                             | _ -> false

(* Or you can combine with evaluate to get to the final value. *)
let t6c = evaluate (desugar (ArithS ("*", NumS 4.2, NumS 2.4))) = Num 10.08


let t6d = try (evaluate (desugar (ArithS ("", BoolS false, NumS 2.5))); false) with Interp "interpErr: not a num" -> true
                                                                              | _ -> false

let t6f = try (evaluate (desugar (ArithS ("-", NumS 2.5, BoolS true))); false) with Interp "interpErr: not a num" -> true
                                                                          | _ -> false

let t6g = evaluate (desugar (ArithS ("/", NumS 2.5, NumS 1.0))) = Num 2.5

let t6h = desugar (ArithS (" ", NumS 3.5, BoolS false)) = ArithC (" ", NumC 3.5, BoolC false)

let t6i = desugar (ArithS ("/.", NumS 0.0, NumS 0.00)) = ArithC ("/.", NumC 0.0, NumC 0.0)
                                                       

let t6j = try (evaluate (desugar (ArithS ("+ ", NumS 0.0, NumS 0.00))); false) with Interp "interpErr: only +, -, *" -> true
                                                                             | _ -> false

let t6i = try (evaluate (desugar (ArithS ("/.", NumS 0.0, ArithS ("+", NumS 0.00, BoolS true)))); false) with Interp "interpErr: not a num" -> true
                                                                             | _ -> false

let t6g = desugar (ArithS ("/.", NumS 0.0, ArithS ("+", NumS 0.00, BoolS true))) = ArithC ("/.", NumC 0.0, ArithC ("+", NumC 0.00, BoolC true))

let t7a = evaluate (CompC ("<", NumC 2.5, NumC 3.6)) = Bool true

(* You can also use interp directly to specify a custom environment. *)
let t7b = try (evaluate (CompC (" ", NumC 2.5, BoolC true)); false) with Interp "interpErr: not a num" -> true
                                                                             | _ -> false

(* Or you can combine with evaluate to get to the final value. *)
let t7c = evaluate (CompC (">=", NumC 4.2, NumC 4.2)) = Bool true


let t7d = try (evaluate (CompC ("", NumC 2.5, NumC 2.5)); false) with Interp "interpErr: only <, <=, >, >=" -> true
                                                                              | _ -> false

let t7e = try (evaluate (CompC ("< ", NumC 2.5, NumC 2.5)); false) with Interp "interpErr: only <, <=, >, >=" -> true
                                                                              | _ -> false


let t7f = evaluate (CompC ("<", NumC 2.5, NumC 2.5)) = Bool false

let t7g = evaluate (CompC (">=", NumC 2.5, NumC 1.0)) = Bool true

let t8a = evaluate (desugar (CompS (">=", NumS 4.2, NumS 4.2))) = Bool true


let t8b = try (evaluate (desugar (CompS ("", NumS 2.5, NumS 2.5))); false) with Interp "interpErr: only <, <=, >, >=" -> true
                                                                              | _ -> false

let t8c = try (evaluate (desugar (CompS ("< ", NumS 2.5, NumS 2.5))); false) with Interp "interpErr: only <, <=, >, >=" -> true
                                                                              | _ -> false


let t8d = desugar (CompS ("< ", NumS 2.5, NumS 2.5)) = CompC ("< ", NumC 2.5, NumC 2.5) 

let t8e = evaluate (desugar (CompS (">=", NumS 2.5, NumS 1.0))) = Bool true

let t8f = evaluate (desugar (CompS (">=", NumS 2.5, NumS 3.0))) = Bool false

let t8e = evaluate (desugar (CompS ("<=", NumS 2.5, NumS 3.0))) = Bool true

let t8f = evaluate (desugar (CompS (">", NumS 2.5, NumS 2.5))) = Bool false

let t8g = evaluate (desugar (CompS ("<", NumS 2.5, NumS 2.5))) = Bool false

let t9a = evaluate (EqC (NumC 4.2, NumC 4.2)) = Bool true

let t9b = evaluate (EqC (NumC 2.5, NumC 2.5)) = Bool true                                                                            

let t9c = evaluate (EqC (NumC 2.5, NumC 2.6)) = Bool false                                                                             

let t9d = evaluate (EqC (NumC 2.5, BoolC true)) = Bool false 

let t9e = evaluate (EqC (IfC (BoolC true, NumC 4.5, NumC 0.9), NumC 4.5)) = Bool true

let t9f = evaluate (EqC (ArithC ("+", NumC 4.2, NumC 4.2000), ArithC("*", NumC 4.2, NumC 2.0))) = Bool true

let t10 = evaluate (desugar (NeqS (ArithS ("+", NumS 4.2, NumS 4.2000), ArithS ("*", NumS 4.2, NumS 2.0)))) = Bool false

let t101 = evaluate (desugar (EqS (ArithS ("+", NumS 4.2, NumS 4.2000), ArithS ("*", NumS 4.2, NumS 2.0)))) = Bool true

let t10a = evaluate (desugar (EqS (NumS 4.2, NumS 4.2))) = Bool true

let t10b = evaluate (desugar (EqS (NumS 2.5, NumS 2.5))) = Bool true                                                                            

let t10c = evaluate (desugar (EqS (NumS 2.5, NumS 2.6))) = Bool false                                                                             

let t10d = desugar (EqS (NumS 2.5, BoolS true)) = EqC (NumC 2.5, BoolC true) 

let t10e = desugar (EqS (IfS (BoolS true, NumS 4.5, NumS 0.9), IfS (NumS 4.5, NumS 4.5, NumS 0.9)))
         = EqC (IfC (BoolC true, NumC 4.5, NumC 0.9), IfC (NumC 4.5, NumC 4.5, NumC 0.9))

let t10f = try (evaluate (desugar (EqS (IfS (BoolS true, NumS 4.5, NumS 0.9), IfS (NumS 4.5, NumS 4.5, NumS 0.9)))); false)
           with Interp "interpErr: only boolean" -> true
           | _ -> false

let t11a = evaluate (desugar (NeqS (NumS 4.2, NumS 4.2))) = Bool false

let t11b = evaluate (desugar (NeqS (NumS 2.5, NumS 4.5))) = Bool true                                                                            

let t11c = evaluate (desugar (NeqS (BoolS false, NumS 2.6))) = Bool true                                                                            

let t11d = desugar (NeqS (NumS 2.5, BoolS true)) = IfC ((EqC (NumC 2.5, BoolC true)), BoolC false, BoolC true) 

let t11e = desugar (NeqS (IfS (BoolS true, NumS 4.5, NumS 0.9), IfS (NumS 4.5, NumS 4.5, NumS 0.9)))
         = IfC (EqC (IfC (BoolC true, NumC 4.5, NumC 0.9), IfC (NumC 4.5, NumC 4.5, NumC 0.9)), BoolC false, BoolC true)

let t11f = try (evaluate (desugar (NeqS (IfS (BoolS true, NumS 4.5, NumS 0.9), IfS (NumS 4.5, NumS 4.5, NumS 0.9)))); false)
           with Interp "interpErr: only boolean" -> true
           | _ -> false

let t11g = evaluate (desugar (NeqS (IfS (BoolS true, NumS 4.5, NumS 0.9), IfS (BoolS false, NumS 4.5, NumS 0.9)))) = Bool true

let t11h = evaluate (desugar (NeqS (NeqS (NumS 2.5, NumS 4.5), NeqS (NumS 2.5, NumS 4.5)))) = Bool false                                                                           

let t11i = evaluate (desugar (NeqS (ArithS ("*", NumS 5.0, NumS 10.0), ArithS ("-", NumS 50.0, NumS 0.0)))) = Bool false                                                                            


