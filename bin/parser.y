%{
	import java.io.*;
	import java.util.ArrayList;
%}

%token NL                    /* newline  */
%token <ival> NUM            /* a number */
%token <sval> IDENT          /* an identifier */
%token PLUS MINUS TIMES DIV  /* operators */
%token NOT AND OR			 /* logical */
%token EQUALS LESSTHAN		 /* compare */
%token LPAR RPAR             /* parethesis */
%token <bval> TRUE FALSE	 /* boolean */

%type <obj> line
%type <obj> expr
%type <obj> exprs

%start line
%%

line:  expr   { prog=(Ast)$1; $$=$1; }
;

expr:
	NUM							{ $$ = new AstNum($1); }
|	IDENT						{ $$ = new AstId($1); }
|	LPAR PLUS exprs RPAR		{ $$ = new AstPrim(Op.ADD,(ArrayList<Ast>)$3); }
|	LPAR MINUS exprs RPAR		{ $$ = new AstPrim(Op.SUB,(ArrayList<Ast>)$3); }
|	LPAR TIMES exprs RPAR		{ $$ = new AstPrim(Op.MUL,(ArrayList<Ast>)$3); }
|	LPAR DIV exprs RPAR			{ $$ = new AstPrim(Op.DIV,(ArrayList<Ast>)$3); }
|	TRUE						{ $$ = new AstBool(true); }
|	FALSE						{ $$ = new AstBool(false); }
|	LPAR NOT exprs RPAR			{ $$ = new AstPrim(Op.NOT,(ArrayList<Ast>)$3); }
|	LPAR AND exprs RPAR			{ $$ = new AstPrim(Op.AND,(ArrayList<Ast>)$3); }
|	LPAR OR exprs RPAR			{ $$ = new AstPrim(Op.OR,(ArrayList<Ast>)$3); }
|	LPAR EQUALS exprs RPAR		{ $$ = new AstPrim(Op.EQUALS,(ArrayList<Ast>)$3); }
|	LPAR LESSTHAN exprs RPAR	{ $$ = new AstPrim(Op.LESSTHAN,(ArrayList<Ast>)$3); }

;
exprs:
expr					{ ArrayList<Ast> r = new ArrayList<Ast>();
						r.add((Ast)$1);

 $$ = r; }
| expr exprs           { ((ArrayList<Ast>)$2).add((Ast)$1); $$ = $2; }
;
%%

	public Ast prog;
	
	private Yylex lexer;
	
	
	private int yylex () {
		int yyl_return = -1;
		try {
			yylval = new ParserVal(0);
			yyl_return = lexer.yylex();
		}
		catch (IOException e) {
			System.err.println("IO error :"+e);
		}return yyl_return;
	}
	
	
	public void yyerror (String error) {
		System.err.println ("Error: " + error);
	}
	
	
	public Parser(Reader r) {
		lexer = new Yylex(r, this);
	}