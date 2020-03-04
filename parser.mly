%{
	open Ast
%}

%token <int> NUM
%token <string> IDENT
%token PLUS MINUS TIMES DIV NOT AND OR EQ LT
%token LPAR RPAR
%token Fleche
%token True False
%token If
%token ECHO
%token Int Bool
%token Points
%token Star
%token Comma
%token LBRA RBRA
%token CONST FUN REC PC

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.stat> stat 
%type <Ast.typeAps > typeAps
%type <Ast.typeAps list> typesAps
%type <Ast.arg > arg
%type <Ast.arg list> args
%type <Ast.dec > dec
%type <Ast.cmds > cmds
%type <Ast.progs> progs



%start progs             /* the entry point */

%%

expr:
	  NUM                        { ASTNum($1) }
	| IDENT                      { ASTId($1) }
	| LPAR PLUS exprs RPAR   { ASTPrim(Ast.Add, $3) }
	| LPAR MINUS exprs RPAR  { ASTPrim(Ast.Sub, $3) }
	| LPAR TIMES exprs RPAR  { ASTPrim(Ast.Mul, $3) }
	| LPAR DIV exprs RPAR    { ASTPrim(Ast.Div, $3) }
	| LPAR NOT exprs RPAR    { ASTPrim(Ast.Not, $3) }
	| LPAR AND exprs RPAR    { ASTPrim(Ast.And, $3) }
	| LPAR OR exprs RPAR     { ASTPrim(Ast.Or, $3) }
	| LPAR EQ exprs RPAR     { ASTPrim(Ast.Eq, $3) }
	| LPAR LT exprs RPAR     { ASTPrim(Ast.Lt, $3) }
	| True 									 { ASTBool(Ast.True) }
	| False 								 { ASTBool(Ast.False) }
	| LPAR If exprs RPAR     { ASTAlt(Ast.If, $3) }
	| LBRA args RBRA expr		 { ASTArgsE($2, $4) }
	| LPAR expr exprs RPAR 	 { ASTExpressions($2, $3) } 
	;

exprs:
	    expr       { [$1] }
	  | expr exprs { $1::$2 }
	  ;

stat:
		ECHO expr 						{ASTEcho($2)}
		;
typeAps:
			Int																{ASTTprim(Ast.Int)} 						
		| Bool															{ASTTprim(Ast.Bool)}
		| LPAR typesAps Fleche typeAps RPAR {ASTArrow($2, Ast.Arrow, $4) }
		;
typesAps :
	    typeAps      											{ [$1] }
	  | typeAps Star typesAps 						{ $1::$3 }
	  ;

arg:
		IDENT Points typeAps									{ASTArg(ASTId($1),Ast.Points, $3)} 
		;
args:
	    arg      														{ [$1] }
	  | arg Comma args 											{ $1::$3 }
		;

	 /* | ASTFun of func * typeAps * arg list * expr */
dec:
		 CONST IDENT typeAps expr 										{ASTConst (Ast.CONST, $2,$3,$4)}
		|FUN IDENT typeAps LBRA args RBRA expr 	  		{ASTFun (Ast.FUN, ASTId($2), $3, $5, $7)}
		|FUN REC IDENT typeAps LBRA args RBRA expr		{ASTRec (Ast.FUN, Ast.REC, ASTId($3), $4, $6, $8)}
		;

		

cmds:
			stat 			   																{ASTTerm($1)}
		| dec PC cmds																	{ASTNTermD($1, Ast.PC, $3)}
		| stat PC cmds  														  {ASTNTermS($1, Ast.PC, $3)}
		;

progs:
		LBRA cmds RBRA 																{ASTProg($2)}
		; 
