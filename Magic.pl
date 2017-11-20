:- use_module(library(clpfd)).

magic(Base, Matrix, Size) :-
    equality(Base, Matrix),
    rule1(Matrix),
    rule2(Matrix),
    transpose(Matrix, Transposed),
    rule1(Transposed),
    rule2(Transposed).



% BlackRule

ruleBlack([]).
ruleBlack([H|T]) :- ruleBlackCheck(H), ruleBlack(T).

ruleBlackCheck([]).
ruleBlackCheck([E1,E2|T]) :- E1=0,E2\=0, ruleBlackCheck(L,T).

% Rule 1 - Checks if there are duplicates in a row or column
rule1([]).
rule1([H|T]) :- rule1Check(H), rule1(T).

rule1Check(L) :- rule1Check(L,L).
rule1Check(_,[]).
rule1Check(L, [H|T]) :- H = 0, rule1Check(L,T).
rule1Check(L, [H|T]) :- H \= 0, count(L,H,R), R=1, rule1Check(L,T).

% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2([]).
rule2([H|T]) :- rule2Check(H), rule2(T).

rule2Check([]) :- true.
rule2Check([_]) :- true.
rule2Check([I1,I2|T]) :- rule2Check([I2|T]), not( (I1=0, I2=0) ). 


% Creates an empty matrix with the same dimensions as the given matrix
constructMatrix([],[]).
constructMatrix([H|T], [A|B]) :- constructRow(H,A), constructMatrix(T,B).
constructRow([],[]).
constructRow([A|T1],[_|T2]) :- constructRow(T1,T2).

% Equality rule
equality([],[]).
equality([H1|T1], [H2|T2]) :- eq(H1,H2), equality(T1,T2).

eq([],[]).
eq([H1|T1], [H2|T2]) :- (H1=1 ; H2=0 ), eq(T1,T2).

% Counts the occuriencies of the element X in the list.
count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).


% Problem solving things...
solvedProblem([[0,2,5,4,3],[4,5,0,1,0],[1,0,3,0,4],[3,4,1,2,5],[0,1,4,0,2]]).
hProblem([
    [1,2,5,4,3],
    [4,5,4,1,1],
    [1,1,3,1,4],
    [3,4,1,2,5],
    [3,1,4,1,2]]).

blank([[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_]]).


test_magic :-
    statistics(runtime, _),
    hProblem(Base),
    constructMatrix(Base, Matrix),
    magic(Base, Matrix, 5),  
    maplist(writeln, Matrix),
    statistics(runtime, [_,T]),
    write('CPU time = '), write(T), write(' msec'), nl, nl, true. 
