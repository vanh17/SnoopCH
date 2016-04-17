# SnoopCH
Racket Interpreter built on Ocaml
There is not much here yet, but below is the original plan:

Build interpreter for part of Racket:

### Syntatic form
- Refers to a top-level, module-level, or local binding, when id is not bound as a transformer (see Expansion). At run-time, the reference evaluates to the value in the location associated with the binding.
When the expander encounters an id that is not bound by a module-level or local binding, it converts the expression to (#%top . id) giving #%top the lexical context of the id; typically, that context refers to #%top. 
- Examples:
       > (define x 10)
       > x
       10
       > (let ([x 5]) x)
       5
       > ((lambda (x) x) 2)
       2

### Procedure Expressions: lambda
- Produces a procedure. The kw-formals determines the number of arguments and which keyword arguments that the procedure accepts.
Considering only the first arg case, a simple kw-formals has one of the following three forms:
`(id ...)`
The procedure accepts as many non-keyword argument values as the number of ids. Each id is associated with an argument value by position.

`(id ...+ . rest-id)`
The procedure accepts any number of non-keyword arguments greater or equal to the number of ids. When the procedure is applied, the ids are associated with argument values by position, and all leftover arguments are placed into a list that is associated to rest-id.

`rest-id`
The procedure accepts any number of non-keyword arguments. All arguments are placed into a list that is associated with rest-id.

More generally, an arg can include a keyword and/or default value. Thus, the first two cases above are more completely specified as follows:

`(arg ...)`
Each arg has the following four forms:

`id`
Adds one to both the minimum and maximum number of non-keyword arguments accepted by the procedure. The id is associated with an actual argument by position.

`[id default-expr]`
Adds one to the maximum number of non-keyword arguments accepted by the procedure. The id is associated with an actual argument by position, and if no such argument is provided, the default-expr is evaluated to produce a value associated with id. No arg with a default-expr can appear before an id without a default-expr and without a keyword.

keyword `id`
The procedure requires a keyword-based argument using keyword. The id is associated with a keyword-based actual argument using keyword.

keyword `[id default-expr]`
The procedure accepts a keyword-based argument using keyword. The id is associated with a keyword-based actual argument using keyword, if supplied in an application; otherwise, the default-expr is evaluated to obtain a value to associate with id.

The position of a keyword arg in kw-formals does not matter, but each specified keyword must be distinct.

`(arg ...+ . rest-id)`
Like the previous case, but the procedure accepts any number of non-keyword arguments beyond its minimum number of arguments. When more arguments are provided than non-keyword arguments among the args, the extra arguments are placed into a list that is associated to rest-id.

The `kw-formals` identifiers are bound in the bodys. When the procedure is applied, a new location is created for each identifier, and the location is filled with the associated argument value. The locations are created and filled in order, with default-exprs evaluated as needed to fill locations.

If any identifier appears in the bodys that is not one of the identifiers in kw-formals, then it refers to the same location that it would if it appeared in place of the lambda expression. (In other words, variable reference is lexically scoped.)

If the procedure produced by lambda is applied to fewer or more by-position or by-keyword arguments than it accepts, to by-keyword arguments that it does not accept, or without required by-keyword arguments, then the exn:fail:contract exception is raised.

The last body expression is in tail position with respect to the procedure body.

- Examples:
> ((lambda (x) x) 10)
10
> ((lambda (x y) (list y x)) 1 2)
'(2 1)
> ((lambda (x [y 5]) (list y x)) 1 2)
'(2 1)

### Local Binding: let, let*, letrec
- `(let ([id val-expr] ...) body ...+)`
- `(let proc-id ([id init-expr] ...) body ...+)`
The first form evaluates the val-exprs left-to-right, creates a new location for each id, and places the values into the locations. It then evaluates the bodys, in which the ids are bound. The last body expression is in tail position with respect to the let form. The ids must be distinct according to bound-identifier=?.
Examples:
> (let ([x 5]) x)
5
> (let ([x 5])
    (let ([x 2]
          [y x])
      (list y x)))
'(5 2)
The second form evaluates the init-exprs; the resulting values become arguments in an application of a procedure (lambda (id ...) body ...+), where proc-id is bound within the bodys to the procedure itself.

Example:
> (let fac ([n 10])
    (if (zero? n)
        1
        (* n (fac (sub1 n)))))
3628800
- `(let* ([id val-expr] ...) body ...+)`
Like let, but evaluates the val-exprs one by one, creating a location for each id as soon as the value is available. The ids are bound in the remaining val-exprs as well as the bodys, and the ids need not be distinct; later bindings shadow earlier bindings.
Example:
> (let* ([x 1]
         [y (+ x 1)])
    (list y x))
'(2 1)
syntax
- `(letrec ([id val-expr] ...) body ...+)`
Like let, including left-to-right evaluation of the val-exprs, but the locations for all ids are created first, all ids are bound in all val-exprs as well as the bodys, and each id is initialized immediately after the corresponding val-expr is evaluated. The ids must be distinct according to bound-identifier=?.
Referencing or assigning to an id before its initialization raises exn:fail:contract:variable. If an id (i.e., the binding instance or id) has an 'undefined-error-name syntax property whose value is a symbol, the symbol is used as the name of the variable for error reporting, instead of the symbolic form of id.

Example:
> (letrec ([is-even? (lambda (n)
                       (or (zero? n)
                           (is-odd? (sub1 n))))]
           [is-odd? (lambda (n)
                      (and (not (zero? n))
                           (is-even? (sub1 n))))])
    (is-odd? 11))
`#t`

### Conditionals: if, cond, and, and or
- `(if test-expr then-expr else-expr)`
Evaluates test-expr. If it produces any value other than #f, then then-expr is evaluated, and its results are the result for the if form. Otherwise, else-expr is evaluated, and its results are the result for the if form. The then-expr and else-expr are in tail position with respect to the if form.
Examples:
> (if (positive? -5) (error "doesn't get here") 2)
2
> (if (positive? 5) 1 (error "doesn't get here"))
1
> (if 'we-have-no-bananas "yes" "no")
"yes"

- `(cond cond-clause ...)`
 
`cond-clause	 	=	 	[test-expr then-body ...+]`
 	 	`|	 	[else then-body ...+]`
 	 	`|	 	[test-expr => proc-expr]`
 	 	`|	 	[test-expr]`

A cond-clause that starts with else must be the last cond-clause.

If no cond-clauses are present, the result is #<void>.

If only a [else then-body ...+] is present, then the then-bodys are evaluated. The results from all but the last then-body are ignored. The results of the last then-body, which is in tail position with respect to the cond form, are the results for the whole cond form.

Otherwise, the first test-expr is evaluated. If it produces #f, then the result is the same as a cond form with the remaining cond-clauses, in tail position with respect to the original cond form. Otherwise, evaluation depends on the form of the cond-clause:

`[test-expr then-body ...+]`
The then-bodys are evaluated in order, and the results from all but the last then-body are ignored. The results of the last then-body, which is in tail position with respect to the cond form, provides the result for the whole cond form.

`[test-expr => proc-expr]`
The proc-expr is evaluated, and it must produce a procedure that accepts one argument, otherwise the exn:fail:contract exception is raised. The procedure is applied to the result of test-expr in tail position with respect to the cond expression.

`[test-expr]`
The result of the test-expr is returned as the result of the cond form. The test-expr is not in tail position.

Examples:
> (cond)
> (cond
    [else 5])
5
> (cond
   [(positive? -5) (error "doesn't get here")]
   [(zero? -5) (error "doesn't get here, either")]
   [(positive? 5) 'here])
'here
> (cond
   [(member 2 '(1 2 3)) => (lambda (l) (map - l))])
'(-2 -3)
> (cond
   [(member 2 '(1 2 3))])
'(2 3)
- `(and expr ...)`
If no exprs are provided, then result is #t.

If a single expr is provided, then it is in tail position, so the results of the and expression are the results of the expr.

Otherwise, the first expr is evaluated. If it produces #f, the result of the and expression is #f. Otherwise, the result is the same as an and expression with the remaining exprs in tail position with respect to the original and form.

Examples:
> (and)
#t
> (and 1)
1
> (and (values 1 2))
1
2
> (and #f (error "doesn't get here"))
#f
> (and #t 5)
5

- `(or expr ...)`
+Combining Tests: and and or in The Racket Guide introduces or.
If no exprs are provided, then result is #f.

If a single expr is provided, then it is in tail position, so the results of the or expression are the results of the expr.

Otherwise, the first expr is evaluated. If it produces a value other than #f, that result is the result of the or expression. Otherwise, the result is the same as an or expression with the remaining exprs in tail position with respect to the original or form.

Examples:
> (or)
#f
> (or 1)
1
> (or (values 1 2))
1
2
> (or 5 (error "doesn't get here"))
5
> (or #f 5)
5


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
- make-string
- Char?
- char->integer
- integer->char

### Datatypes
#### 1 Booleans and Equality
True and false booleans are represented by the values #t and #f, respectively, though operations that depend on a boolean value typically treat anything other than #f as true. The #t value is always eq? to itself, and #f is always eq? to itself.
- `(boolean? v) → boolean?`
  v : any/c
Returns #t if v is #t or #f, #f otherwise.
Examples:
`> (boolean? #f)`
`#t`
`> (boolean? #t)`
`#t`
`> (boolean? 'true)`
`#f`
procedure
- `(not v) → boolean?`
  v : any/c
Returns #t if v is #f, #f otherwise.
Examples:
`> (not #f)`
`#t`
`> (not #t)`
`#f`
`> (not 'we-have-no-bananas)`
`#f`
procedure
- `(equal? v1 v2) → boolean?`
  v1 : any/c
  v2 : any/c
Two values are equal? if and only if they are eqv?, unless otherwise specified for a particular datatype.
Datatypes with further specification of equal? include strings, byte strings, pairs, mutable pairs, vectors, boxes, hash tables, and inspectable structures. In the last six cases, equality is recursively defined; if both v1 and v2 contain reference cycles, they are equal when the infinite unfoldings of the values would be equal. See also gen:equal+hash and prop:impersonator-of.

Examples:
`> (equal? 'yes 'yes)`
`#t`
> (equal? 'yes 'no)
`#f`
`> (equal? (expt 2 100) (expt 2 100))`
`#t`
> (equal? 2 2.0)
`#f`
`> (equal? (make-string 3 #\z) (make-string 3 #\z))`
`#t`

#### 2 Numbers
All numbers are complex numbers. Some of them are real numbers, and all of the real numbers that can be represented are also rational numbers, except for +inf.0 (positive infinity), +inf.f (single-precision variant), -inf.0 (negative infinity), -inf.f (single-precision variant), +nan.0 (not-a-number), and +nan.f (single-precision variant). Among the rational numbers, some are integers, because round applied to the number produces the same number.

Orthogonal to those categories, each number is also either an exact number or an inexact number. Unless otherwise specified, computations that involve an inexact number produce inexact results. Certain operations on inexact numbers, however, produce an exact number, such as multiplying an inexact number with an exact 0.

In the case of complex numbers, either the real and imaginary parts are both exact or inexact with the same precision, or the number has an exact zero real part and an inexact imaginary part; a complex number with an exact zero imaginary part is a real number.

Inexact real numbers are implemented as either single- or double-precision IEEE floating-point numbers—the latter by default, and the former only when a computation starts with numerical constants specified as single-precision numbers. Inexact real numbers that are represented as double-precision floating-point numbers are flonums.

Inexact numbers can be coerced to exact form, except for the inexact numbers +inf.0, +inf.f, -inf.0, -inf.f, +nan.0, and +nan.f, which have no exact form. Dividing a number by exact zero raises an exception; dividing a non-zero number other than +nan.0 or +nan.f by an inexact zero returns +inf.0, +inf.f, -inf.0 or -inf.f, depending on the sign and precision of the dividend. The +nan.0 value is not = to itself, but +nan.0 is eqv? to itself, and +nan.f is similarly eqv? but not = to itself. Conversely, (= 0.0 -0.0) is #t, but (eqv? 0.0 -0.0) is #f, and the same for 0.0f0 and -0.0f0 (which are single-precision variants). The datum -nan.0 refers to the same constant as +nan.0, and -nan.f is the same as +nan.f.

- Arithmetic
`(+ z ...) → number?`
  z : number?
Returns the sum of the zs, adding pairwise from left to right. If no arguments are provided, the result is 0.
Examples:
`> (+ 1 2)`
`3`
`> (+ 1.0 2+3i 5)`
`8.0+3.0i`
`> (+)`
`0`
- `(- z) → number?`
  z : number?
`(- z w ...+) → number?
  z : number?
  w : number?`
When no ws are supplied, returns (- 0 z). Otherwise, returns the subtraction of the ws from z working pairwise from left to right.
Examples:
`> (- 5 3.0)
2.0
> (- 1)
-1
> (- 2+7i 1 3)
-2+7i`

- `(* z ...) → number?`
  z : number?
Returns the product of the zs, multiplying pairwise from left to right. If no arguments are provided, the result is 1. Multiplying any number by exact 0 produces exact 0.
Examples:
`> (* 2 3)
6
> (* 8.0 9)
72.0
> (* 1+2i 3+4i)
-5+10i`

- `(/ z) → number?`
  z : number?
(/ z w ...+) → number?
  z : number?
  w : number?
When no ws are supplied, returns (/ 1 z). Otherwise, returns the division of z by the ws working pairwise from left to right.
If z is exact 0 and no w is exact 0, then the result is exact 0. If any w is exact 0, the exn:fail:contract:divide-by-zero exception is raised.

Examples:
`> (/ 3 4)
3/4
> (/ 81 3 3)
9
> (/ 10.0)
0.1
> (/ 1+2i 3+4i)
11/25+2/25i`

- Number Comparison
- `(= z w ...+) → boolean?`
  z : number?
  w : number?
Returns #t if all of the arguments are numerically equal, #f otherwise. An inexact number is numerically equal to an exact number when the exact coercion of the inexact number is the exact number. Also, 0.0 and -0.0 are numerically equal, but +nan.0 is not numerically equal to itself.
Examples:
`> (= 1 1.0)
#t
> (= 1 2)
#f
> (= 2+3i 2+3i 2+3i)
#t`

- `(< x y ...+) → boolean?`
  x : real?
  y : real?
Returns #t if the arguments in the given order are strictly increasing, #f otherwise.
Examples:
`> (< 1 1)
#f
> (< 1 2 3)
#t
> (< 1 +inf.0)
#t
> (< 1 +nan.0)
#f`

- `(<= x y ...+) → boolean?`
  x : real?
  y : real?
Returns #t if the arguments in the given order are non-decreasing, #f otherwise.
Examples:
`> (<= 1 1)
#t
> (<= 1 2 1)
#f`

- `(> x y ...+) → boolean?`
  x : real?
  y : real?
Returns #t if the arguments in the given order are strictly decreasing, #f otherwise.
Examples:
`> (> 1 1)
#f
> (> 3 2 1)
#t
> (> +inf.0 1)
#t
> (> +nan.0 1)
#f`

- `(>= x y ...+) → boolean?`
  x : real?
  y : real?
Returns #t if the arguments in the given order are non-increasing, #f otherwise.
Examples:
`> (>= 1 1)
#t
> (>= 1 2 1)
#f`

#### 2 String Constructors, Selectors, and Mutators

procedure
`(string? v) → boolean?`
  v : any/c
Returns #t if v is a string, #f otherwise.
Examples:
`> (string? "Apple")
#t`
`> (string? 'apple)
#f`

- `(make-string k [char]) → string?`
  k : exact-nonnegative-integer?
  char : char? = #\nul
Returns a new mutable string of length k where each position in the string is initialized with the character char.
Example:
`> (make-string 5 #\z)
"zzzzz"`
procedure
- `(string char ...) → string?`
  char : char?
Returns a new mutable string whose length is the number of provided chars, and whose positions are initialized with the given chars.
Example:
`> (string #\A #\p #\p #\l #\e)
"Apple"`

#### 3 Characters and Scalar Values
- `(char? v) → boolean?`
  v : any/c
Return #t if v is a character, #f otherwise.
procedure
- `(char->integer char) → exact-integer?`
  char : char?
Returns a character’s code-point number.
Example:
`> (char->integer #\A)
65`
procedure
- `(integer->char k) → char?`
Return the character whose code-point number is k. For k less than 256, the result is the same object for the same k.
Example:
`> (integer->char 65)
#\A`

#### 4 Pairs and Lists
A pair combines exactly two values. The first value is accessed with the car procedure, and the second value is accessed with the cdr procedure. Pairs are not mutable (but see Mutable Pairs and Lists).

A list is recursively defined: it is either the constant null, or it is a pair whose second value is a list.

A list can be used as a single-valued sequence (see Sequences). The elements of the list serve as elements of the sequence. See also in-list.

Cyclic data structures can be created using only immutable pairs via read or make-reader-graph. If starting with a pair and using some number of cdrs returns to the starting pair, then the pair is not a list.

- `(pair? v) → boolean?`
  v : any/c
Returns #t if v is a pair, #f otherwise.
Examples:
`> (pair? 1)
#f
> (pair? (cons 1 2))
#t
> (pair? (list 1 2))
#t
> (pair? '(1 2))
#t
> (pair? '())
#f`

- `(cons a d) → pair?`
  a : any/c
  d : any/c
Returns a newly allocated pair whose first element is a and second element is d.
Examples:
`> (cons 1 2)
'(1 . 2)
> (cons 1 '())
'(1)`
procedure
- `(car p) → any/c`
  p : pair?
Returns the first element of the pair p.
Examples:
`> (car '(1 2))
1
> (car (cons 2 3))
2`
procedure
- `(cdr p) → any/c`
  p : pair?
Returns the second element of the pair p.
Examples:
`> (cdr '(1 2))
'(2)
> (cdr '(1))
'()`

- `(list v ...) → list?`
  v : any/c
Returns a newly allocated list containing the vs as its elements.
Examples:
`> (list 1 2 3 4)
'(1 2 3 4)
> (list (list 1 2) (list 3 4))
'((1 2) (3 4))`





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
