%TYPE
type(_, num(_),int).

type(_,true,bool).
type(_,false,bool).

type(_,[],void).

%SYM
expr(G,x,t):-type(G,x,t). % not sure

%ABS
abs(G,e,t):-
    type(G,e,t).

%APP

% EXPR
exprAdd(G,add(x,y),int):-
    type(G,x,int), 
    type(G,y,int).

exprSub(G,sub(x,y),int):-
    type(G,x,int), 
    type(G,y,int).

exprMult(G,mult(x,y),int):-
    type(G,x,int), 
    type(G,y,int).

exprDiv(G,div(x,y),int):-
    type(G,x,int), 
    type(G,y,int).

exprEq(G,eq(x,y),bool):-
    type(G,x,int), 
    type(G,y,int).

exprLt(G,lt(x,y),bool):-
    type(G,x,int), 
    type(G,y,int).

exprAnd(G,and(x,y),bool):-
    type(G,x,bool), 
    type(G,y,bool).

exprOr(G,or(x,y),bool):-
    type(G,x,bool), 
    type(G,y,bool).

exprNot(G,not(x),bool):-
    type(G,x,bool).

typeExprVar(G,var(x),t):- 
    type(G,x,ident).


exprCond(G,if(e1,e2,e3),t):- 
    type(G,e1,bool),
    type(G,e2,t),
    type(G,e3,t).

%STAT

stat(_,echo(e),void):-
 type(_,e,int). 

%DEC
%dec(G,const(x,t,:-
 %type(G,e,t).

%CMDS

%PROG
