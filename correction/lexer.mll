{
	open Parser
	        (* The type token is defined in parser.mli *)
	exception Eof
}
rule token = parse
	[' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
	| ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
	| "add"            { PLUS }
	| "sub"            { MINUS }
	| "mul"            { TIMES }
	| "div"            { DIV }
	| '('              { LPAR }
	| ')'              { RPAR }
	| ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
	| eof              { raise Eof }

