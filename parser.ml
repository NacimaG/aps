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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
	open Ast
# 37 "parser.ml"
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
  280 (* LBRA *);
  281 (* RBRA *);
  282 (* CONST *);
  283 (* FUN *);
  284 (* REC *);
  285 (* PC *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\002\000\003\000\004\000\004\000\004\000\005\000\005\000\
\006\000\007\000\007\000\008\000\008\000\008\000\009\000\009\000\
\009\000\010\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\001\000\001\000\004\000\004\000\004\000\
\001\000\002\000\002\000\001\000\001\000\005\000\001\000\003\000\
\003\000\001\000\003\000\004\000\007\000\008\000\001\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\002\000\000\000\012\000\013\000\000\000\
\019\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\021\000\
\000\000\000\000\000\000\033\000\032\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\000\000\
\000\000\018\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\014\000\016\000\025\000\027\000\015\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\022\000\029\000\000\000\030\000"

let yydgoto = "\002\000\
\046\000\047\000\008\000\061\000\062\000\036\000\037\000\009\000\
\010\000\004\000"

let yysindex = "\005\000\
\241\254\000\000\242\254\000\000\016\255\009\255\001\255\243\254\
\248\254\253\254\000\000\000\000\042\255\000\000\000\000\021\255\
\000\000\251\254\251\254\022\255\242\254\242\254\000\000\016\255\
\016\255\016\255\016\255\016\255\016\255\016\255\016\255\016\255\
\016\255\016\255\004\255\010\255\002\255\251\254\000\000\000\000\
\016\255\011\255\251\254\000\000\000\000\016\255\024\255\025\255\
\026\255\029\255\043\255\047\255\048\255\049\255\050\255\051\255\
\064\255\251\254\021\255\016\255\012\255\065\255\000\000\021\255\
\054\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\251\254\251\254\055\255\021\255\000\000\068\255\016\255\058\255\
\000\000\000\000\016\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\059\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\060\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\074\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\075\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\251\255\042\000\000\000\239\255\009\000\000\000\202\255\000\000\
\254\255\000\000"

let yytablesize = 90
let yytable = "\017\000\
\041\000\042\000\019\000\005\000\079\000\001\000\038\000\034\000\
\003\000\083\000\018\000\006\000\007\000\039\000\040\000\021\000\
\011\000\012\000\044\000\045\000\022\000\023\000\035\000\043\000\
\058\000\065\000\060\000\013\000\020\000\088\000\014\000\015\000\
\059\000\081\000\064\000\063\000\067\000\068\000\069\000\016\000\
\078\000\070\000\011\000\012\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\013\000\080\000\071\000\
\014\000\015\000\033\000\072\000\073\000\074\000\075\000\076\000\
\086\000\016\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\077\000\084\000\082\000\087\000\
\089\000\090\000\091\000\031\000\026\000\092\000\017\000\066\000\
\023\000\085\000"

let yycheck = "\005\000\
\018\000\019\000\002\001\018\001\059\000\001\000\012\001\013\000\
\024\001\064\000\002\001\026\001\027\001\019\001\020\001\029\001\
\001\001\002\001\021\000\022\000\029\001\025\001\002\001\002\001\
\021\001\043\000\025\001\012\001\028\001\084\000\015\001\016\001\
\023\001\022\001\024\001\041\000\013\001\013\001\013\001\024\001\
\058\000\013\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\060\000\013\001\
\015\001\016\001\017\001\013\001\013\001\013\001\013\001\013\001\
\082\000\024\001\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\013\001\024\001\014\001\025\001\
\013\001\087\000\025\001\025\001\025\001\091\000\013\001\046\000\
\014\001\081\000"

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
  LBRA\000\
  RBRA\000\
  CONST\000\
  FUN\000\
  REC\000\
  PC\000\
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
# 38 "parser.mly"
                              ( ASTNum(_1) )
# 208 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                              ( ASTId(_1) )
# 215 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 40 "parser.mly"
                          ( ASTPrim(Ast.Add, _3) )
# 222 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 41 "parser.mly"
                          ( ASTPrim(Ast.Sub, _3) )
# 229 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 42 "parser.mly"
                          ( ASTPrim(Ast.Mul, _3) )
# 236 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 43 "parser.mly"
                          ( ASTPrim(Ast.Div, _3) )
# 243 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 44 "parser.mly"
                          ( ASTPrim(Ast.Not, _3) )
# 250 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 45 "parser.mly"
                          ( ASTPrim(Ast.And, _3) )
# 257 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 46 "parser.mly"
                          ( ASTPrim(Ast.Or, _3) )
# 264 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 47 "parser.mly"
                          ( ASTPrim(Ast.Eq, _3) )
# 271 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 48 "parser.mly"
                          ( ASTPrim(Ast.Lt, _3) )
# 278 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                  ( ASTBool(Ast.True) )
# 284 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                  ( ASTBool(Ast.False) )
# 290 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 51 "parser.mly"
                          ( ASTAlt(Ast.If, _3) )
# 297 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                         ( ASTArgsE(_2, _4) )
# 305 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 53 "parser.mly"
                          ( ASTExpressions(_2, _3) )
# 313 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                ( [_1] )
# 320 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 58 "parser.mly"
                ( _1::_2 )
# 328 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                  (ASTEcho(_2))
# 335 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                      (ASTTprim(Ast.Int))
# 341 "parser.ml"
               : Ast.typeAps ))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                       (ASTTprim(Ast.Bool))
# 347 "parser.ml"
               : Ast.typeAps ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typeAps list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typeAps ) in
    Obj.repr(
# 67 "parser.mly"
                                      (ASTArrow(_2, Ast.Arrow, _4) )
# 355 "parser.ml"
               : Ast.typeAps ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typeAps ) in
    Obj.repr(
# 70 "parser.mly"
                             ( [_1] )
# 362 "parser.ml"
               : Ast.typeAps list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typeAps ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typeAps list) in
    Obj.repr(
# 71 "parser.mly"
                                 ( _1::_3 )
# 370 "parser.ml"
               : Ast.typeAps list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typeAps ) in
    Obj.repr(
# 75 "parser.mly"
                               (ASTArg(ASTId(_1),Ast.Points, _3))
# 378 "parser.ml"
               : Ast.arg ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg ) in
    Obj.repr(
# 78 "parser.mly"
                            ( [_1] )
# 385 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 79 "parser.mly"
                               ( _1::_3 )
# 393 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typeAps ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
                                      (ASTConst (Ast.CONST, _2,_3,_4))
# 402 "parser.ml"
               : Ast.dec ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typeAps ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 84 "parser.mly"
                                              (ASTFun (Ast.FUN, ASTId(_2), _3, ASTArgsE(_5, _7)))
# 412 "parser.ml"
               : Ast.dec ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typeAps ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                                              (ASTRec (Ast.FUN, Ast.REC, ASTId(_3), _4, ASTArgsE(_6, _8)))
# 422 "parser.ml"
               : Ast.dec ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 91 "parser.mly"
                              (ASTTerm(_1))
# 429 "parser.ml"
               : Ast.cmds ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds ) in
    Obj.repr(
# 92 "parser.mly"
                                (ASTNTermD(_1, Ast.PC, _3))
# 437 "parser.ml"
               : Ast.cmds ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds ) in
    Obj.repr(
# 93 "parser.mly"
                                  (ASTNTermS(_1, Ast.PC, _3))
# 445 "parser.ml"
               : Ast.cmds ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds ) in
    Obj.repr(
# 97 "parser.mly"
                                 (ASTProg(_2))
# 452 "parser.ml"
               : Ast.progs))
(* Entry progs *)
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
let progs (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.progs)
