%===============================================================================
%== UPMC/master/info/4I506 -- Janvier 2016                                    ==
%== SU/FSI/master/info/MU4IN503 -- Janvier 2020                               ==
%== Analyse des programmes et sémantiques                                     ==
%===============================================================================
%== Fichier: typrog.pl                                                        ==
%== Vérification de type                                                      ==
%===============================================================================

%%
% Type ::= int | bool | void | arrow([Type],Type)

%%
% Contextes: [(Var,Type)]

% Un contexte de variables -- modifier selon les tests
varCtx([]).

%addtoCon([],TS,TS).
%addtoCon([(_,T)|L],LS,TS):- append([T],LS,TS),
%					   addtoCon(L,TS,Y).


% Contexte initial
primCtx([
	(add,arrow([int,int],int)),
	(sub,arrow([int,int],int)),
	(mul,arrow([int,int],int)),
	(div,arrow([int,int],int)),
	(eq,arrow([int,int],bool)),
	(lt,arrow([int,int],bool)),
    (and,arrow([bool,bool],bool)),
    (or,arrow([bool,bool],bool)),
    (not,bool,bool) ]).
    


% Accès
fetchType([(X,T)|_],X,T) :-!.
fetchType([_|G],X,T) :-
	fetchType(G,X,T).

extract(X,[X|_]).
extract(X,[_|XS]) :- extract(X,XS).




appends([],TS,TS).
appends([(_,T)|L],LS,TS):- append([T],LS,TS), appends(L,TS,_).
%%
% Expr
typeExpr(_,num(_),int).



%BOOL
typeExpr(_,true,bool).

typeExpr(_,false,bool).
typeExpr(_,[],void).


typeExpr(G,sym(X),T) :- fetchType(G,X,T).


typeExpr(G,if(E1,E2,E3),T) :-
	typeExpr(G,E1,bool), 
	typeExpr(G,E2,T), 
	typeExpr(G,E3,T).


%Expressions 
typeExpr(G,expressions([E|ES]),([T|TS])) :- 
	write("exprs\n"),
	typeExpr(G,E,T),
	write("ex1\n"),
	typeExprs(G,ES,TS).

%APP

typeExpr(G,app(F,ES),T) :- 
	typeExpr(G,F,arrow(TS,T)),
	typeExprs(G,ES,TS).




typeExpr(C,var(X),T):- extract((var(X),T),C).


% (G,E,T)
%ABS
typeExpr(G,abs(AS,E),arrow(_,T)) :- 
	append(G,AS,G2),
	typeExpr(G2,E,T).


%Expr
typeExprs(_,[],[]).
typeExprs(G,[E|ES],[T|TS]) :- 
	typeExpr(G,E,T), 
	typeExprs(G,ES,TS).


typeStat(G,echo(E),void) :-
	write("stat\n"), 
	typeExpr(G,E,_),
	write("out\n").    





% Dec ::= CONST ident Type Expr
typeDec(G,const(X;T,E),[(X,T)|G]) :-
	write("totoA\n"),
	typeExpr(G,E,T).

%Fun


typeDec(G,fun(var(X),T,A,E),[(var(X),arrow(_,T))|G]) :-
	append(G,A,G2) , 
	typeExpr(G2,E,T).

/*typeDec(G,fun(var(X),T,AS,E),[(var(X),arrow(TS,T))|G]) :-
	write("fun\n"),
	append(G,AS,G1),
	write("aperererep\n"),
	typeExpr(G1,E,TS).
*/
%FuncRec (FUN REC IDENT typeAps [args] expr)

typeDec(G,funRec(var(X),T,AS,E),[(var(X),arrow(TS,T))|G]):-
	write("funrec\n"),
	append(AS,[],TS),
	write("app1\n"),
	append(G,AS,G2),
	write("app2\n"),
	append(G2,(var(X),arrow(TS,T)),G3),
	write("append2\n"), 
	typeExpr(G3,E,T),
	write("finRec\n").

% ABS 
%typeABS (G,[E|ES],G2) :-
%	typeExpr (G,E)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CMDS
% CMDS ::= STATsym
typeCmds(G,echo(X),void) :- 
	write("cmds \n"),
	typeStat(G,echo(X),void).

% CMDS ::= DEC ; CMDS
typeCmds(G,termD(D,S),T) :- 
	write("cmdsDect\n"),
	typeDec(G,D,G2),
	write("toto\n"),
	typeCmds(G2,S,T).

% CMDS ::= STAT; CMDS
typeCmds(G,statCmds(St,Cs),T) :- 
	typeStat(G,St,T),
	typeCmds(G,Cs,T).





% Prog ::= [ Cmds ]
typeProg(G,prog(P)) :- 
	write("prog \n"),
	typeCmds(G,P,void),
	write("after prog\n").



%
typeCheck(P,ok) :- varCtx(GP), primCtx(G0), append(GP,G0,G),
		   write("check\n"),
		   typeProg(G,P),
		   write("apres \n").
typeCheck(_,ko).       


%
exitCode(ok) :- halt(0).
exitCode(_) :- halt(1).

%
main_stdin :-
	read(user_input,T),
    typeCheck(T,R),
    print(R),
    nl,
    exitCode(R).

