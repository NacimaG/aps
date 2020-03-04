%TYPE
type(_, num(_),int).
type(_,true,bool).
type(_,false,bool).
type(_,[],void)

%SYM
expr(G,x,t):-type(G,x,t). % not sure

%ABS

%APP

% EXPR
expr(G,add(x,y),int):-
    type(G,x,int), 
    type(G,y,int).

expr(G,sub(x,y),int):-
    type(G,x,int), 
    type(G,y,int).

expr(G,mult(x,y),int):-
    type(G,x,int), 
    type(G,y,int).

expr(G,div(x,y),int):-
    type(G,x,int), 
    type(G,y,int).

expr(G,Eq(x,y),bool):-
    type(G,x,int), 
    type(G,y,int).

expr(G,Lt(x,y),bool):-
    type(G,x,int), 
    type(G,y,int).

expr(G,and(x,y),bool):-
    type(G,x,bool), 
    type(G,y,bool).

expr(G,or(x,y),bool):-
    type(G,x,bool), 
    type(G,y,bool).

expr(G,not(x),bool):-
    type(G,x,bool), 

typeExprVar(G,var(x),T):- type(G,x,ident).



type(G,if(cond,do1,doelse),T):- 
    type(_,cond,bool),
    type(G,do1,T),
    type(G,doelse,T).

%STAT

stat(_,ECHO(e),void):-
    type(_,e,int). 

%DEC
dec(G,CONST(x,t,e),G[x:t]):-
    type(G,e,t).

%CMDS
%PROG