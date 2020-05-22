open Ast
let rec print_expr e =
	match e with
	  ASTNum n -> Printf.printf"num(%d)" n
	| ASTId x -> Printf.printf"var(%s)" x
	| ASTPrim(op, es) -> (
		Printf.printf"app(sym(%s)" (string_of_op op);
		Printf.printf"[";
		print_exprs es;
		Printf.printf"])"
	)
	|ASTBool b -> Printf.printf "%s" (string_of_bool b)
	
	|ASTAlt(alt, res) ->(
		Printf.printf "if";
		Printf.printf "(";
		print_exprs res;
		Printf.printf ")"	
	) 
	|ASTArgsE(ars,e) ->(
		Printf.printf "(";
		print_args ars;
		Printf.printf ", ";
		print_expr e;
		Printf.printf ")";
	)
	|ASTExpressions(e,es) ->(
		Printf.printf "(";
		print_expr e;
		print_char ',';
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
		Printf.printf "fleche(";
		print_types ts;
		Printf.printf ", " ;
		print_type t;
		Printf.printf")"
	)

	and print_types_elem ts =
	match ts with
	[]->()
	| [t] -> print_type t
	| t::ts -> (
		print_type t;
		Printf.printf ", ";
		print_types ts;
		)

	and print_types ts =
	match ts with
	[] -> ()
	| [t] -> print_type t
	| t::ts -> (
		Printf.printf "[";
		print_type t;
		Printf.printf ", ";
		print_types_elem ts;
		Printf.printf "]";
		)

	and print_arg a =
		match a with  
		ASTArg (a,p,t) -> ( 
			Printf.printf "(";
			print_expr a ;
			Printf.printf ",";
			print_type t; 
			Printf.printf ")";
		)
	and	print_args ars =
		match ars with
		[] -> ()
		| [a] -> print_arg a
		| a::ars -> (
			print_arg a;
			Printf.printf ", ";
			print_args ars;
			)

	and print_dec d =
		match d with
		ASTConst (c, i, t, e) -> (
			Printf.printf "%s" (string_of_const c);
			Printf.printf "(";
			Printf.printf "%s" i;
			Printf.printf ", ";
			print_type t;
			Printf.printf ", ";
			print_expr e;
			Printf.printf ")";
		)
		| ASTFun (f, i, t, ars, e) -> (
			Printf.printf "%s" (string_of_fun f);
			Printf.printf "(";
			print_expr i;
			Printf.printf ", ";
			print_type t;
			Printf.printf ", [";
			print_args ars;
			Printf.printf "], ";
			print_expr e;
			Printf.printf")";
		)
		| ASTRec (f, r, i, t, ars, e) -> (
			Printf.printf "%s" (string_of_fun f);
			Printf.printf "(";
			Printf.printf "%s" (string_of_rec r);
			Printf.printf ", ";
			print_expr i;
			Printf.printf ", ";
			print_type t;
			Printf.printf ", [";
			print_args ars;
			Printf.printf "], ";
			print_expr e;
			Printf.printf")";
		)
	
	and print_stat e = 
				match e with 
				 ASTEcho arg -> (Printf.printf "echo(";
											  print_expr arg;
												Printf.printf ")"
				)
	
	let rec print_cmds s =
		match s with
			ASTTerm s -> (
				Printf.printf "\t";
				print_stat s
			)
			| ASTNTermD (d, p, c) -> (
				Printf.printf "\t";
				print_dec d;
				Printf.printf "%s\n" (string_of_pc p);
				print_cmds c;
			)
			| ASTNTermS (s,p,c) -> (
				Printf.printf "\t";
				print_stat s;
				Printf.printf "%s\n" (string_of_pc p);
				print_cmds c;
			)

	let print_progs p =
		match p with
		ASTProg cs -> (
		   print_string "prog(\n";	
			print_cmds cs;
			print_string "\n)"
			)
	;;

	let fname = Sys.argv.(1) in
	let ic = open_in fname in
	try
		let lexbuf = Lexing.from_channel ic in
		let d = try Parser.progs Lexer.token lexbuf with Lexer.Eof -> raise Not_found in

		print_progs d;
		print_string ".\n"
	with Lexer.Eof ->
		exit 0 
	(*
	print_string"eof(2)\n"
	| Not_found -> print_string "eof(1)\n"*)