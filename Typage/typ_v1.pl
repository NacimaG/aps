
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
fetchType([(X,T)|_],X,T) :- !.
fetchType([_|G],X,T) :- fetchType(G,X,T).


%variable en maj
%type en min

%NUM
type(_, num(_),int).
%BOOL
type(_,true,bool).
type(_,false,bool).
type(_,[],void).

%SYM
sym(G,X,T):-
    expr(G,X,T);
    type(G,X,bool).

%ABS
abs(G,[(Head,Type)|Tail],T):-
    type(G,Head,Type),
    abs(G,Tail,T).

%APP
% app(G,,t).


%IF
exprCond(G,if(Cond,Do,Else),T):- 
    type(G,Cond,bool),
    type(G,Do,T),
    type(G,Else,T).

%ECHO
stat(_,echo(E),void):-
 type(_,E,int). 


%CONST
dec(G,const(X,T,E),Env):-
    type(G,E,T).
    


%Expr
expr(G,add(X,Y),int):-
    type(G,X,int), 
    type(G,Y,int).

expr(G,sub(X,Y),int):-
    type(G,X,int), 
    type(G,Y,int).

expr(G,mult(X,Y),int):-
    type(G,X,int), 
    type(G,Y,int).

expr(G,div(X,Y),int):-
    type(G,X,int), 
    type(G,Y,int).

expr(G,eq(X,Y),bool):-
    type(G,X,int), 
    type(G,Y,int).

expr(G,lt(X,Y),bool):-
    type(G,X,int), 
    type(G,Y,int).

expr(G,and(X,Y),bool):-
    type(G,X,bool), 
    type(G,Y,bool).

expr(G,or(X,Y),bool):-
    type(G,X,bool), 
    type(G,Y,bool).

expr(G,not(X),bool):-
    type(G,X,bool).

expr(G,var(X),T):- 
    type(G,X,T).




%DEC
%dec(G,const(X,T,E):-
 %type(G,E,T).


%END

%CMDS

%PROG

%
main_stdin :-
    read(user_input,T),
    typeur(T,R),
    print(R),
    nl,
    exitCode(R).

