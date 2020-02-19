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
	;;

	let fname = Sys.argv.(1) in
	let ic = open_in fname in
	try
		let lexbuf = Lexing.from_channel ic in
		let e = Parser.expr Lexer.token lexbuf in
		print_expr e;
		print_char '\n'
	with Lexer.Eof ->
	exit 0