type token =
  | FLOAT of (float)
  | COMPOP of (string)
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | OR
  | AND
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQ
  | NEQ
  | DBLSEMI

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.exprS
