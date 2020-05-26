open Ast
open Env
type value = Im of int  
            | B of bool 
            | FClo of expr * (string list) 



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
let rec eval_cmds env cm = 
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

and eval_stat env s =
  match s with
    | ASTEcho (e) -> (
	match (eval_expr env e) with
        Im n -> ( Printf.printf"%d\n" n )
      | B b -> match b with 
            true -> ( Printf.printf"true \n")
          | false -> ( Printf.printf"false \n" )
     
      )
      
and eval_arg env a = 
  match a with 
  |ASTArg(a,p,t) -> 
    (
      let env1 = (add env a t) in 
        env1
    )
and eval_args env ars =
  match ars with
  [a] -> eval_arg a 
  e::es -> (eval_arg e):: eval_args es   

and eval_expr env e =
  match e with
      Ast.ASTNum n -> Im n
    | ASTId x -> get env x
    | ASTPrim(f,es) -> (
        (pi f) (Array.map (eval_expr env) (Array.of_list es))
        )(*eval_exprs env es*)
    | ASTAlt(alt,e1,e2,e3) -> (match (eval_expr env e1) with 
                                B true -> (eval_expr env e2)
                              | B false -> (eval_expr env e3) 
    
    )
   (* | ASTArgsE(ars,e) -> *)
    

and eval_dec env d = 
match d with 
(*CONST Ident Type eXPR)*)
  ASTConst (e1,e2,e3) ->(
                let Im n = (eval_expr env e3) in 
                add env e1 (Im n)
  )
  | ASTFun (f,i,t,ars,e) ->
       let arg= eval_args ars in
         add env i FClo(e,arg,env,i)

 
and eval_exprs env e =
  match e with 
  [] -> Array.of_list []
  (*[x] -> (Array.of_list [(eval_expr env  x)] )*)
 (*|x::xs-> Array.of_list ((eval_expr env x)::(Array.to_list(eval_exprs env xs)))*)


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
