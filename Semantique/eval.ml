   
open Ast

type value = Im of int  

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


let rec eval_prog p =
  match p with
      Prog s -> eval_stat s

and eval_stat s =
  match s with
    | Ast.Echo e -> (
	match (eval_expr e) with
	    Im n -> ( Printf.printf"%d\n" n )
      )
      
and eval_expr e =
  match e with
      Ast.Num n -> Im n
    | Ast.Prim(f,es) -> (pi f) (Array.map eval_expr (Array.of_list es)) 

let parse_prog ic =
  let lexbuf = Lexing.from_channel ic in
    try
      Parser.prog Lexer.token lexbuf 
    with
	Parsing.Parse_error -> (
	    Printf.fprintf stderr "Erreur syntaxique autour du caractère %d\n"
	                          (lexbuf.Lexing.lex_curr_pos);
	    raise Parsing.Parse_error)
;;

if Array.length Sys.argv < 2 then
  Printf.fprintf stderr "Usage: %s <source-file>\n" Sys.argv.(0)
else (
  let ic = open_in Sys.argv.(1) in
  let ast = parse_prog ic in
    (eval_prog ast) 
)
