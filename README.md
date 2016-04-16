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
- String?



To compile:

Linux/Mac:
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c types.mli parser.mli lexer.ml parser.ml types.ml driver.ml
ocamlc -o snoopch lexer.cmo parser.cmo types.cmo driver.cmo
./snoopch

Windows:
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c types.mli parser.mli lexer.ml parser.ml types.ml driver.ml
./snoopch.exe

You can use it interactively as above. Or you can write a "program" in any file, then run it as input to the interpreter by:

./lang < sample_input.txt

It is worth it to write some sample programs as you go by and load them that way, for quick testing of your lexer and parser.


To run the interpreter tests:

ocamlc -c types.mli parser.mli lexer.ml parser.ml types.ml driver.ml
Start utop
Enter: #load "types.cmo";;
Enter: #use "tests.ml";;
