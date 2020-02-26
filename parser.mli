type token =
  | NUM of (int)
  | IDENT of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | NOT
  | AND
  | OR
  | EQ
  | LT
  | LPAR
  | RPAR
  | Fleche
  | True
  | False
  | If
  | ECHO
  | Int
  | Bool
  | Points
  | Star
  | Comma
  | LBRA
  | RBRA
  | CONST
  | FUN
  | REC
  | PC

val progs :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.progs
