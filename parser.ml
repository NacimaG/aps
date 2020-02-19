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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
	open Ast
# 31 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIV *);
  263 (* NOT *);
  264 (* AND *);
  265 (* OR *);
  266 (* EQ *);
  267 (* LT *);
  268 (* LPAR *);
  269 (* RPAR *);
  270 (* Fleche *);
  271 (* True *);
  272 (* False *);
  273 (* If *);
  274 (* ECHO *);
  275 (* Int *);
  276 (* Bool *);
  277 (* Points *);
  278 (* Star *);
  279 (* Comma *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\003\000\004\000\004\000\004\000\005\000\005\000\006\000\007\000\
\007\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\001\000\001\000\004\000\001\000\002\000\
\002\000\001\000\001\000\005\000\001\000\003\000\003\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\018\000\019\000\
\023\000\000\000\000\000\000\000\000\000\022\000\000\000\020\000"

let yydgoto = "\002\000\
\000\000\000\000\000\000\010\000\011\000\004\000\000\000"

let yysindex = "\001\000\
\001\255\000\000\239\254\000\000\244\254\244\254\000\000\000\000\
\000\000\240\254\247\254\244\254\244\254\000\000\253\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\254\254\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\252\255\255\255\000\000\000\000"

let yytablesize = 12
let yytable = "\006\000\
\009\000\001\000\003\000\005\000\013\000\012\000\007\000\008\000\
\015\000\016\000\014\000\021\000"

let yycheck = "\012\001\
\005\000\001\000\002\001\021\001\014\001\022\001\019\001\020\001\
\013\000\013\001\012\000\014\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  NOT\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  LPAR\000\
  RPAR\000\
  Fleche\000\
  True\000\
  False\000\
  If\000\
  ECHO\000\
  Int\000\
  Bool\000\
  Points\000\
  Star\000\
  Comma\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 32 "parser.mly"
                              ( ASTNum(_1) )
# 136 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
                              ( ASTId(_1) )
# 143 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 34 "parser.mly"
                          ( ASTPrim(Ast.Add, _3) )
# 150 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 35 "parser.mly"
                          ( ASTPrim(Ast.Sub, _3) )
# 157 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 36 "parser.mly"
                          ( ASTPrim(Ast.Mul, _3) )
# 164 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 37 "parser.mly"
                          ( ASTPrim(Ast.Div, _3) )
# 171 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 38 "parser.mly"
                          ( ASTPrim(Ast.Not, _3) )
# 178 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 39 "parser.mly"
                          ( ASTPrim(Ast.And, _3) )
# 185 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 40 "parser.mly"
                          ( ASTPrim(Ast.Or, _3) )
# 192 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 41 "parser.mly"
                          ( ASTPrim(Ast.Eq, _3) )
# 199 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 42 "parser.mly"
                          ( ASTPrim(Ast.Lt, _3) )
# 206 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                  ( ASTBool(Ast.True))
# 212 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                  ( ASTBool(Ast.False))
# 218 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 45 "parser.mly"
                          (ASTAlt(Ast.If, _3))
# 225 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                ( [_1] )
# 232 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 50 "parser.mly"
                ( _1::_2 )
# 240 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                  (ASTEcho(_2))
# 247 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                      (ASTTprim(Ast.Int))
# 253 "parser.ml"
               : Ast.typeAps ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                       (ASTTprim(Ast.Bool))
# 259 "parser.ml"
               : Ast.typeAps ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typeAps list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typeAps ) in
    Obj.repr(
# 59 "parser.mly"
                                      (ASTArrow(_2, Ast.Arrow, _4) )
# 267 "parser.ml"
               : Ast.typeAps ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typeAps ) in
    Obj.repr(
# 62 "parser.mly"
                             ( [_1] )
# 274 "parser.ml"
               : Ast.typeAps list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typeAps ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typeAps list) in
    Obj.repr(
# 63 "parser.mly"
                                 ( _1::_3 )
# 282 "parser.ml"
               : Ast.typeAps list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typeAps ) in
    Obj.repr(
# 67 "parser.mly"
                               (ASTArg(ASTId(_1),Ast.Points, _3))
# 290 "parser.ml"
               : Ast.arg ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg ) in
    Obj.repr(
# 70 "parser.mly"
                            ( [_1] )
# 297 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 71 "parser.mly"
                               ( _1::_3 )
# 305 "parser.ml"
               : Ast.arg list))
(* Entry arg *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let arg (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.arg )
