%{
	open Ast
%}

%token <int> NUM
%token <string> IDENT
%token PLUS MINUS TIMES DIV NOT AND OR EQ LT
%token LPAR RPAR
%token True False
%token If
%token ECHO

%type <Ast.expr> expr
%type <Ast.expr list> exprs

%start expr             /* the entry point */

%%

expr:
	  NUM                        { ASTNum($1) }
	| IDENT                      { ASTId($1) }
	| LPAR PLUS exprs RPAR   { ASTPrim(Ast.Add, $3) }
	| LPAR MINUS exprs RPAR  { ASTPrim(Ast.Sub, $3) }
	| LPAR TIMES exprs RPAR  { ASTPrim(Ast.Mul, $3) }
	| LPAR DIV exprs RPAR    { ASTPrim(Ast.Div, $3) }

	;
	exprs :
	    expr       { [$1] }
	  | expr exprs { $1::$2 }
	  ;
