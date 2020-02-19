open Ast
let rec print_expr e =
	match e with
	ASTNum n -> Printf.printf"num(%d)" n
	| ASTId x -> Printf.printf"var(%s)" x
	| ASTPrim(op, es) -> (
		Printf.printf"%s" (string_of_op op);
		Printf.printf"([";
		print_exprs es;
		Printf.printf"])"
	)
	|ASTBool b -> Printf.printf "bool(%s)" (string_of_bool b)
	
	|ASTAlt(alt, res) ->(
		Printf.printf "if";
		Printf.printf "([";
		print_exprs res;
		Printf.printf "[)"	
	) 


	and print_exprs es =
	match es with
	[] -> ()
	| [e] -> print_expr e
	| e::es -> (
				print_expr e;
				print_char ',';
				print_exprs es
				)

	let rec print_stat e = 
	match e with 
 	ASTEcho arg -> (Printf.printf "ECHO(";
								 (print_expr arg);
									Printf.printf ")")
									
	
	(* let rec print_type t =
	match t with 
		ASTTprim t -> Printf.printf "%s" (string_of_types t)
		| ASTArrow (ts , t )-> (Printf.printf "("; 
										 print_types ts;
										 Printf.printf ")"; 

										)								 
	and print_types ts =
	match ts with 
		[] -> ()
		| [t] -> print_type t 
		| t::ts -> (print_type t ;
								Printf.printf "->";
								print_types ts )
	
	;; *)

	and print_type t =
	match t with
	ASTTprim t -> Printf.printf "%s" (string_of_type t)
	| ASTArrow (ts, arrow, t) -> (
		Printf.printf "(";
		print_types ts;
		Printf.printf "->" ;
		print_type t;
		Printf.printf")"
	)

	and print_types ts =
	match ts with
	[] -> ()
	| [t] -> print_type t
	| t::ts -> (
		Printf.printf "(";
		print_type t;
		Printf.printf "*";
		print_types ts;
		Printf.printf ")";
		)

	let print_arg a =
		match a with  
		ASTArg (a,p,t) -> ( print_expr a ;
			Printf.printf "%s" (string_of_p p);
			print_type t; 

		)
	;;
	let fname = Sys.argv.(1) in
	let ic = open_in fname in
	try
		let lexbuf = Lexing.from_channel ic in
(*		let e = Parser.expr Lexer.token lexbuf in 
		let e = Parser.stat Lexer.token lexbuf in 
		let e = Parser.typeAps Lexer.token lexbuf in*)

		let e = Parser.arg Lexer.token lexbuf in
(*		print_expr e;
		print_stat e; *)
		print_arg e;
		print_char '\n'
	with Lexer.Eof ->
	exit 0