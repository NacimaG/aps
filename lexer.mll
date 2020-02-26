{
	open Parser
	        (* The type token is defined in parser.mli *)
	exception Eof
}
rule token = parse
	[' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
	| ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
	| "ECHO"					 { ECHO }
	| "bool"					 { Bool }
	| "int"						 { Int }
	| "add"            { PLUS }
	| "sub"            { MINUS }
	| "mul"            { TIMES }
	| "div"            { DIV }
	| "not"						 { NOT }
	| "and"			  		 { AND}
	| "or"						 { OR }
	| "eq"             { EQ }
	| "lt"             { LT }
	| "->"						 { Fleche }	
	| "CONST"					 { CONST }
	| "FUN"						 { FUN }
	| "REC"						 { REC }
	| '*'							 { Star }
	| ':'							 { Points }
	| ',' 						 { Comma }
	| '('              { LPAR }
	| ')'              { RPAR }
	| '['							 { LBRA }
	| ']'						   { RBRA }
	| ';'							 { PC }
	| "true"					 { True }
	| "false"          { False }
	| "if"             { If }
	| ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
	| eof              { raise Eof }

