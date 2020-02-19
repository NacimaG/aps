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

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.stat> stat 
%type <Ast.typeAps > typeAps
%type <Ast.typeAps list> typesAps
%type <Ast.arg > arg
%type <Ast.arg list> args


%start arg             /* the entry point */

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
	| True 									 { ASTBool(Ast.True)}
	| False 								 { ASTBool(Ast.False)}
	| LPAR If exprs RPAR     {ASTAlt(Ast.If, $3)}
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

		
