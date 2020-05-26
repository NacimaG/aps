
%%
% Type ::= int | bool | void | arrow([Type],Type)

%%
% Contextes: [(Var,Type)]

% Un contexte de variables -- modifier selon les tests
varCtx([]).

% Contexte initial
primCtx([(add,arrow([int,int],int)),
	(sub,arrow([int,int],int)),
	(mul,arrow([int,int],int)),
	(div,arrow([int,int],int))
       ]).

% Accès
type([(X,T)|_],X,T) :- !.
type([_|G],X,T) :- type(G,X,T).

%%
% Expr
texpr(_,num(_),int).

texpr(G,sym(X),T) :- type(G,X,T).
    
texpr(G,if(Cond,Do,Else),T) :- texpr(G,Cond,bool),
                               texpr(G,Do,T), 
                               texpr(G,Else,T).

texpr(G,app(F,ES),T) :- texpr(G,F,arrow(TS,T)), 
                        texprs(G,ES,TS).

%
texprs(_,[],[]).
texprs(G,[E|ES],[T|TS]) :- texpr(G,E,T), 
                           texprs(G,ES,TS).

%
tStat(G,echo(E),void) :- texpr(G,E,int).    

%
tProg(G,prog(S)) :- tStat(G,S,void).

%
  

typeCheck(P,ok) :- varCtx(GP), primCtx(G0), append(GP,G0,G),
		   tProg(G,P).
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


