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
    type(G,X,num);
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
    


% expr
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
