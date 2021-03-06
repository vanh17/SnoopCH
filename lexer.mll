{
  open Parser
  exception Eof
  exception Unrecognized
}

let any = _
let digit = ['0'-'9']
let sign = ['+' '-']
let frac = '.' digit+
let exp = ['e' 'E'] sign? digit+
let white = [' ' '\t' '\n' '\r']+ | "//" ([^ '\n' '\r'])*
let newline = '\n' | '\r' | "\r\n"
let dblsemi = ";;"
let float = sign? (digit+ '.' | digit* frac) exp?
let int = sign? (digit+)
let fract = sign? (digit+) '/' (digit+)
let pinf = "+inf.0"
let ninf = "-inf.0"
let nan = "+nan.0"
let true = ("true" | "#t")
let false = ("false" | "#f")
let comp = (">" | ">=" | "<" | "<=")
let var = (['A'-'Z'] | ['a' - 'z'] | digit | sign | '.' | '?' | '/' | '>' | '<' | '=' | '\\' | '@' | '$' | '%' | '~' | ',' | '_' | '*')+
let st  = (['A'-'Z'] | ['a' - 'z'] | digit | sign | '.' | '?' | '/' | '>' | '<' | '\\' | '@' | '$' | '%' | '~' | ',' | '_' | '#' | ''' | '|' | ':')
let stg = '"' (st | ' ' | '[' | ']' | '(' | ')' | '~' | '&' | '!' | var)+ '"'
let chr = '#' '\\' st+ 

rule token = parse
  | ";;" (st | ' ' | '\t' | '~' | '&' | '*' | var | '"' | '[' | ']' | '(' | ')' | ';' | '!' | ''' | '`')+ newline  { token lexbuf } 
  | white       { token lexbuf }
  | newline     { token lexbuf }
  | true        { TRUE }
  | false       { FALSE }
  | dblsemi     { DBLSEMI }
  | (''')* (float as x)  { FLOAT (float_of_string x) }
  | (''')* (int as x)    { INT (int_of_float (float_of_string x)) }
  | (''')* (sign? (digit+) as x) '/' (digit+ as y)  { FRAC (int_of_float (float_of_string x), int_of_string y) } 
  | (''')* (sign? (digit+) as x1) '/' ((digit+) as x2 ) (['+' '-'] (digit+) as x3) '/' ((digit+) as x4) "i" { COMF (int_of_float (float_of_string x1), int_of_string x2, int_of_float (float_of_string x3), int_of_string x4) }
  | (''')* (sign? (digit+) as x1) '/' ((digit+) as x2 ) (['+' '-'] (digit+ '.' | digit* frac) exp? as x3) "i" { COMFN (int_of_float (float_of_string x1), int_of_string x2, float_of_string x3) }
  | (''')* (float+ as x1) (['+' '-'] (digit+) as x2) '/' ((digit+) as x3) "i" { COMNF (float_of_string x1, int_of_float (float_of_string x2), int_of_string x3) }
  | (''')* (float+ as x1) (['+' '-'] (digit+ '.' | digit* frac) exp? as x2) "i" { COMN (float_of_string x1, float_of_string x2) }
  | (''')* (int+ as x1) (['+' '-'] (digit+) as x2) "i" { COMF (int_of_float (float_of_string x1), 1, int_of_float (float_of_string x2), 1) }
  | (''')* (int+ as x1) (['+' '-'] (digit+) as x2) '/' ((digit+) as x3) "i" { COMF (int_of_float (float_of_string x1), 1, int_of_float (float_of_string x2), int_of_string x3) }
  | (''')* (int+ as x1) (['+' '-'] (digit+ '.' | digit* frac) exp? as x2) "i" { COMFN (int_of_float (float_of_string x1), 1, float_of_string x2) }
  | (''')* (float+ as x1) (['+' '-'] (digit+) as x2) "i" { COMNF (float_of_string x1, int_of_float (float_of_string x2), 1) }
  | (''')* (sign? (digit+) as x1) '/' ((digit+) as x2 ) (['+' '-'] (digit+) as x3) "i" { COMF (int_of_float (float_of_string x1), int_of_string x2, int_of_float (float_of_string x3), 1) }
  | (''')* (int+ as x1) (['+' '-'] as x2) "i" { COMF (int_of_float (float_of_string x1), 1, int_of_string (if x2 = '+' then "1" else "-1"), 1) }
  | (''')* (float+ as x1) (['+' '-'] as x2) "i" { COMNF (float_of_string x1, int_of_string (if x2 = '+' then "1" else "-1"), 1) }
  | (''')* (sign? (digit+) as x1) '/' ((digit+) as x2 ) (['+' '-'] as x3) "i" { COMF (int_of_float (float_of_string x1), int_of_string x2, int_of_string (if x3 = '+' then "1" else "-1"), 1) }
  | "cond"      { COND }
  | "lambda"    { FUN }
  | "list"      { LIST }
  | "'"         { SQUOTE }
  | "car"       { CAR }
  | "cdr"       { CDR }
  | "null"      { NULL }
  | "cons"      { CONS }
  | "("         { OPAREN }
  | ")"         { CPAREN }
  | "["         { OSB }
  | "]"         { CSB }
  | "+inf.0"    { PINF }
  | "-inf.0"    { NINF }
  | "+nan.0"    { NAN }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "or"        { OR }
  | "and"       { AND }
  | "not"       { NOT }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "."         { DOT }
  | "/"         { DIVIDE }
  | "="         { EQ }
  | "!="        { NEQ }
  | "String?"   { ISSTRING }
  | "Char?"     { ISCHAR }
  | "make-string" { MAKESTRING }
  | "string"    { STRINGFROMLST }
  | "let"       { LET }
  | "let*"      { LETS }
  | "letrec"    { LETREC }
  | "map"       { MAP }
  | "foldr"     { FOLDR }
  | "foldl"     { FOLDL }
  | "filter"    { FILTER }
  | "remove"    { REMOVE }
  | "define"    { DEFINE }
  | "equal?"    { EQUAL}
  | "char->integer" { CHARTOINT }
  | "integer->char" { INTTOCHAR }
  | "null?"     { ISNULL }
  | "number?"   { ISNUM }
  | "error"     { ERROR }
  | "write"     { WRITE }
  | "begin"     { BEGIN }
  | "begin0"    { BEGIN0 }
  | comp as s   { COMPOP s}
  | var as s    { VAR s }
  | stg as s    { STRING s }
  | ''' (st+ as s)                    { SYMBOL  s }
  | '#' '\\' (st as s)                { CHAR s }
  | '#' '\\' ("null" | "nul" as s)    { CHARNULL s }
  | eof                               { raise Eof }
