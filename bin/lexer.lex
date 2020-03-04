%%

%byaccj

%{
	private Parser yyparser;
	
	public Yylex(java.io.Reader r, Parser yyparser) {
		this(r);
		this.yyparser = yyparser;
	}
%}

nums = -?[0-9]+
ident = [a-z][a-zA-Z0-9]*
nls  = \n | \r | \r\n


%%

/* boolean */
"true" {return Parser.TRUE; }
"false" {return Parser.FALSE; }

/* logical */
"not"  { return Parser.NOT; }
"and"  { return Parser.AND; }
"or"   { return Parser.OR; }

/* compare */
"eq"  { return Parser.EQUALS; }
"lt"  { return Parser.LESSTHAN; }

/* operators */
"add"  { return Parser.PLUS; }
"sub"  { return Parser.MINUS; }
"mul"  { return Parser.TIMES; }
"div"  { return Parser.DIV; }

/* parenthesis */
"("  { return Parser.LPAR; }
")"  { return Parser.RPAR; }

/* newline */
{nls}   { return 0; } //{ return Parser.NL; }

/* float */
{nums}  { yyparser.yylval = new ParserVal(Integer.parseInt(yytext()));
		return Parser.NUM; }
		
{ident} { yyparser.yylval = new ParserVal(yytext());
		return Parser.IDENT;
}

/* whitespace */
[ \t]+ { }

\b     { System.err.println("Sorry, backspace doesn’t work"); }

/* error fallback */
[^]    { System.err.println("Error: unexpected character ’"+yytext()+"’"); return -1; }