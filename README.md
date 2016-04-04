# SnoopCH
Racket Interpreter built on Ocaml
There is not much here yet, but below is the original plan:

Build interpreter for small part of Racket:

- Parsing
- Real numbers (floating point), infinity, nan
- Integers, operations to take arbitrary number of values
- Booleans, conditionals, "cond", case/switch?
- cons, car, cdr, null, lists, map, filter, foldl/r
- let, let*, letrec, define
- begin, (set! ?), box, set-box!
- symbols, strings
- lambdas (variable number of arguments), function calls

extras:

- macros?
- garbage collection?
