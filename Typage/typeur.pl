
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
    (div,arrow([int,int],int)),
    (eq,arrow([int,int],bool)),
    (lt,arrow([int,int],bool)),
    (and,arrow([bool,bool],bool)),
    (or,arrow([bool,bool],bool)),
    (not,bool,bool) ]).

% Accès
type([(X,T)|_],X,T) :- !.
type([_|G],X,T) :- type(G,X,T).

%%
% Expr
expr(_,num(_),int).

expr(G,sym(X),T) :- type(G,X,T).
    
expr(G,if(Cond,Do,Else),T) :- expr(G,Cond,bool),
                              expr(G,Do,T), 
                              expr(G,Else,T).

expr(G,app(F,ES),T) :- expr(G,F,arrow(TS,T)), 
                       exprs(G,ES,TS).

%
exprs(_,[],[]).
exprs(G,[E|ES],[T|TS]) :- expr(G,E,T), 
                          exprs(G,ES,TS).

%
tStat(G,echo(E),void) :- expr(G,E,int).    

%
tProg(G,prog(S)) :- tStat(G,S,void).

%
typeCheck(P,ok) :- varCtx(GP), 
                   primCtx(G0), 
                   append(GP,G0,G),
		   tProg(G,P).
typeCheck(_,ko).    


%
exitCode(ok) :- halt(0).
exitCode(_) :- halt(1).

%
main_stdin :-
    read(user_input,T),
    typeur(T,R),
    print(R),
    nl,
    exitCode(R).


