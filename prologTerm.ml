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
	|ASTArgsE(ars,e) ->(
		Printf.printf "[";
		print_args ars;
		Printf.printf "]";
		print_expr e
	)
	|ASTExpressions(e,es) ->(
		Printf.printf "(";
		print_expr e;
		print_exprs es;
		Printf.printf ")"
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

	and print_arg a =
		match a with  
		ASTArg (a,p,t) -> ( 
			Printf.printf "(";
			print_expr a ;
			Printf.printf "%s" (string_of_p p);
			print_type t; 
			Printf.printf ")";
		)
	and	print_args ars =
		match ars with
		[] -> ()
		| [a] -> print_arg a
		| a::ars -> (
			Printf.printf "(";
			print_arg a;
			Printf.printf ",";
			print_args ars;
			Printf.printf ")";
			)

	and print_dec d =
		match d with
		ASTConst (c, i, t, e) -> (
			Printf.printf "(";
			Printf.printf "%s" (string_of_const c);
			Printf.printf " ";
			Printf.printf "%s" i;
			Printf.printf " ";
			print_type t;
			Printf.printf " ";
			print_expr e;
			Printf.printf ")";
		)
		| ASTFun (f, i, t, ars) -> (
			Printf.printf "(";
			Printf.printf "%s" (string_of_fun f);
			Printf.printf " ";
			print_expr i;
			Printf.printf " ";
			print_type t;
			Printf.printf " ";
			print_expr ars;
			Printf.printf ")";
		)
		| ASTRec (f, r, i, t, ars) -> (
			Printf.printf "(";
			Printf.printf "%s" (string_of_fun f);
			Printf.printf " ";
			Printf.printf "%s" (string_of_rec r);
			Printf.printf " ";
			print_expr i;
			Printf.printf " ";
			print_type t;
			Printf.printf " ";
			print_expr ars;
			Printf.printf ")";
		)
	
	and print_stat e = 
				match e with 
				 ASTEcho arg -> (Printf.printf "echo(";
											 (print_expr arg);
												Printf.printf ")"
				)
	
	let rec print_cmds s =
		match s with
			ASTTerm s -> (
				print_stat s
			)
			| ASTNTermD (d, p, c) -> (
				print_dec d;
				Printf.printf " ";
				Printf.printf "%s " (string_of_pc p);
				print_cmds c;
			)
			| ASTNTermS (s,p,c) -> (
				print_stat s;
				Printf.printf " ";
				Printf.printf "%s " (string_of_pc p);
				print_cmds c;
			)

	let print_progs p =
		match p with
		ASTProg cs -> (
		   print_string"prog(";	
			print_cmds cs;
			print_string ")"
			)
	;;

	let fname = Sys.argv.(1) in
	let ic = open_in fname in
	try
		let lexbuf = Lexing.from_channel ic in
(*		let e = Parser.expr Lexer.token lexbuf in 
		let e = Parser.stat Lexer.token lexbuf in 
		let e = Parser.typeAps Lexer.token lexbuf in
		let e = Parser.arg Lexer.token lexbuf in*)
		let d = try Parser.progs Lexer.token lexbuf with Lexer.Eof -> raise Not_found in

(*		print_expr e;
		print_stat e; *)
		print_progs d;
		print_char '\n'
	with Lexer.Eof ->
	print_string"eof(2)\n"
	| Not_found -> print_string "eof(1)\n"