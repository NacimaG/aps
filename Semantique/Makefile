

CC = ocamlc
FLAG = -c
YACC = ocamlyacc
LEX = ocamllex
I = -I ../Syntaxe




eval : ../Syntaxe/ast.cmo ../Syntaxe/parser.cmo ../Syntaxe/lexer.cmo env.ml eval.ml
	${CC} -o eval ${I} ast.cmo parser.cmo lexer.cmo env.ml eval.ml

clean :
	rm -f *~
	rm -f *.cmi *.cmo
	rm -f eval

.SUFFIXES : .mll .mly .mli .cmi .ml .cmo

.mly.ml : 
	$(YACC) $<

.mly.mli : 
	$(YACC) $<

.mll.ml : 
	$(LEX) $<

.ml.cmi : 
	$(CC) $(I) $(FLAG) $<

.mli.cmi : 
	$(CC) $(I) $(FLAG) $<

.ml.cmo : 
	$(CC) $(I) $(FLAG) $<
