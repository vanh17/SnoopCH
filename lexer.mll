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
let pinf = "+inf.0"
let ninf = "-inf.0"
let nan = "+nan.0"
let true = "true" | "#t"
let false = "false" | "#f"
let comp = ">" | ">=" | "<" | "<="

rule token = parse
  | white       { token lexbuf }
  | newline     { token lexbuf }
  | true        { TRUE }
  | false       { FALSE }
  | dblsemi     { DBLSEMI }
  | float as x  { FLOAT (float_of_string x) }
  | int as x    { INT (int_of_string x) }
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
  | "/"         { DIVIDE }
  | "=="        { EQ }
  | "!="        { NEQ }
  | comp as s   { COMPOP s}
  | eof         { raise Eof }
  | any         { raise Unrecognized }
