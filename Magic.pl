:- use_module(library(clpfd)).

magic(Matrix, Size) :-
    transpose(Matrix, Transposed),
    equality(Matrix),
    rule1(Matrix),
    rule2(Matrix),
    rule1(Transposed),
    rule2(Transposed).

% Rule 1 - Checks if there are duplicates in a row or column
rule1([]).
rule1([H|T]) :- rule1Check(H), rule1(T).

rule1Check(L) :- rule1Check(L,L).
rule1Check(_,[]).
rule1Check(L, [ [_,S] |T]) :- S=0, rule1Check(L,T).
rule1Check(L, [ [V,S] |T]) :- S\=0, noDuplicates(L,V), rule1Check(L,T).

noDuplicates(L,V) :- noDuplicates(L,V,0). 
noDuplicates([],_,1).
noDuplicates([],_,0).
noDuplicates(_,_,2) :- false.
noDuplicates([ [V,S] |T], X, R) :- V=X, S\=0, A is R+1, noDuplicates(T,X,A).
noDuplicates([ [V,S] |T], X, R) :- (V\=X ; S=0), noDuplicates(T,X,R).


% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2([]).
rule2([H|T]) :- rule2Check(H), rule2(T).

rule2Check([]) :- true.
rule2Check([_]) :- true.
rule2Check([ [_,0], [_,0] |T]) :- rule2Check([_|T]), false.
rule2Check([ [_,0], [_,1] |T]) :- rule2Check([_|T]).
rule2Check([ [_,1], [_,0] |T]) :- rule2Check([_|T]).
rule2Check([ [_,1], [_,1] |T]) :- rule2Check([_|T]).


% Creates an empty matrix with the same dimensions as the given matrix
constructMatrix([],[]).
constructMatrix([H|T], [A|B]) :- constructRow(H,A), constructMatrix(T,B).
constructRow([],[]).
constructRow([A|T1],[ [A,_] |T2]) :- constructRow(T1,T2).

% Equality rule
equality([]).
equality([H|T]) :- eq(H), equality(T).

eq([]).
eq([ [_,S] |T]) :- (S=1 ; S=0 ), eq(T).

% Counts the occuriencies of the element X in the list.
count([],_,0).
count([ [V,_] |T], X, Z):- V\=X, count(T,X,Z).
count([ [X,1] |T], X, Y):- count(T,X,Z), Y is 1+Z.
count([ [_,0] |T], X, Z):- count(T,X,Z).



% Problem solving things...
solvedProblem([
    [0,2,5,4,3],
    [4,5,0,1,0],
    [1,0,3,0,4],
    [3,4,1,2,5],
    [0,1,4,0,2]]).

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
    magic(Matrix, 5),  
    maplist(writeln, Matrix),
    statistics(runtime, [_,T]),
    write('CPU time = '), write(T), write(' msec'), nl, nl, true. 


    % [ [1,0],[1,1],[2,1],[2,0],[1,0] ]