open Ast
open Env
open Printf

type t = (string * v) list

and v =
    Im of int
  | B of bool 
  | ASTPrim of (v array -> v)
  | FClo of expr * (string list) * t
  | RClo of expr * (string list)* t * string

let pi f vs =
  match f with
    | Add -> (
	match vs.(0),vs.(1) with
      Im n1, Im n2 -> Im (n1+n2)
      )
    | Sub -> (
	match vs.(0),vs.(1) with
	    Im n1, Im n2 -> Im (n1-n2)
      )
    | Mul -> (
	match vs.(0),vs.(1) with
	    Im n1, Im n2 -> Im (n1*n2)
      )
    | Div -> (
	match vs.(0),vs.(1) with
	    Im n1, Im n2 -> Im (n1/n2)
      )
    | Not -> (
  match vs.(0) with 
      B n -> B (not n)
    )
    | And -> (
	match vs.(0),vs.(1) with
	    B n1, B n2 -> B (n1 && n2)
    )
    | Or -> (
  match vs.(0),vs.(1) with
      B n1, B n2 -> B (n1 || n2)
    )
    | Eq -> (
  match vs.(0),vs.(1) with
      Im n1, Im n2 -> B (n1 = n2)
    )
    | Lt -> (
  match vs.(0),vs.(1) with
      Im n1, Im n2 -> B (n1 < n2)
    )

  (**********************************)      




 
let rec eval_args ars =
  match ars with
  [] -> [] 
  |ASTArg (e,p,t)::es -> (let ASTId x = e in x) :: (eval_args es)

  (*(eval_expr env e)::(eval_args env es)   *)

and eval_expr env e =
  match e with
    ASTNum n -> Im(v(n))
    | ASTId x -> get env x
    | ASTPrim(f,es) -> (
        (pi f) (Array.map (eval_expr env) (Array.of_list es))
        )(*eval_exprs env es*)
    | ASTAlt(alt,e1,e2,e3) -> (match (eval_expr env e1) with 
                                B true -> (eval_expr env e2)
                              | B false -> (eval_expr env e3) 
    
    )
    | ASTExpressions(e,es)-> (
       (* match e with *)
        let ASTId(x) = e in 
          let e1 =  get env x  in 
            match e1 with
              FClo(e1,args,env1) ->(
                let env2= adds env  args (eval_exprs env es) in 
                  eval_expr env2 e1
              )
              | RClo(e,args,envC,x) -> (
                   let env2 =  adds env args (eval_exprs env es) in
                      eval_expr env2 e
              
          )
    )
and eval_exprs env e =
  match e with 
  [] -> []
  (*[x] -> (Array.of_list [(eval_expr env  x)] )*)
  |x::xs->(eval_expr env x)::(eval_exprs env xs)
 
  and v n =	n


  and eval_stat env s =
  match s with 
    ASTEcho(e1) -> let tmp= (eval_expr env e1) in 
      match tmp with 
        | Im(n) -> Printf.printf "%d\n" n


  and eval_dec env d = 
  match d with 
  (*CONST Ident Type eXPR)*)
    ASTConst(e1,e2,e3) ->(
                  let Im n = (eval_expr env e3) in 
                  add env e1 (Im n)
    )
    | ASTFun (i,t,ars,e)->(
          let ASTId x = i in 
          let arg= (eval_args ars) in
              add env x (FClo (e,arg,env))
              )
    |ASTRec(e,e0,e1,e2,e3,e4) -> (
      let ASTId x= e1 in 
      let args = (eval_args e3) in
		      add env x (RClo(e4,args,env,x))
    )
   

and eval_cmds env cm = 
    match cm with 
      | ASTTerm s -> eval_stat env s
      | ASTNTermD (d, c) -> (
        let env1 = eval_dec env d in
          eval_cmds env1 c 
      )
      | ASTNTermS(s,p,c) ->(
          eval_stat env s ;
          eval_cmds env c
        )
  


and eval_prog env p =
  match p with
    ASTProg (cs)->( eval_cmds env cs 

    )


let parse_prog ic =
  let lexbuf = Lexing.from_channel ic in
    try
      Parser.progs Lexer.token lexbuf 
    with
	Parsing.Parse_error -> (
	    Printf.fprintf stderr "Erreur syntaxique autour du caractère %d\n"
	                          (lexbuf.Lexing.lex_curr_pos);
	    raise Parsing.Parse_error)
;;
let _ = 
  if Array.length Sys.argv < 2 then
    Printf.fprintf stderr "Usage: %s <source-file>\n" Sys.argv.(0)
  else (
    let ic = open_in Sys.argv.(1) in
    let ast = parse_prog ic in
      eval_prog [] ast 
  )
  ;;
