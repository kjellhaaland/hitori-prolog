:- use_module(library(clpfd)).

phase2(Rows, Result, Size) :-
    equality(Rows,Result),
    rule1(Result),
    rule2(Result).

% Rule 1 - Checks if there are duplicates in a row or column
rule1(M) :- rule1Check(M), transpose(M,T), rule1Check(T).

rule1Check([]).
rule1Check([H|T]) :- checkLine2(H), rule1Check(T).

checkLine2(L) :- checkLine2(L,L).
checkLine2(_,[]).
checkLine2(L, [H|T]) :- H = 0, checkLine2(L,T).
checkLine2(L, [H|T]) :- H \= 0, noDuplicates(L,H), checkLine2(L,T).


noDuplicates(L,V) :- noDuplicates(L,V,0). 
noDuplicates([],_,1).
noDuplicates([],_,0).
noDuplicates(_,_,2) :- false.
noDuplicates([V|T], X, R) :- V=X, A is R+1, noDuplicates(T,X,A).
noDuplicates([V|T], X, R) :- V\=X , noDuplicates(T,X,R).


% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2(M) :- rule2Check(M), transpose(M,T), rule2Check(T).

rule2Check([]).
rule2Check([H|T]) :- checkLine(H), rule2Check(T).

checkLine([]) :- true.
checkLine([_]) :- true.
checkLine([I1,I2|T]) :- checkLine([I2|T]), not(blackPair(I1,I2)). 

blackPair(I1,I2) :- I1 == 0, I2 == 0.



% Creates an empty matrix with the same dimensions as the given matrix
constructMatrix([],[]).
constructMatrix([H|T], [A|B]) :- constructRow(H,A), constructMatrix(T,B).
constructRow([],[]).
constructRow([_|T1],[_|T2]) :- constructRow(T1,T2).


% Equality rule
equality([],[]).
equality([H1|T1], [H2|T2]) :- eq(H1,H2), equality(T1,T2).

eq([],[]).
eq([H1|T1], [H2|T2]) :- (H1=H2  ; H2=0 ), eq(T1,T2).

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

test_magic :-
  statistics(runtime, _),
  hProblem(Base),
  constructMatrix(Base, Matrix),
  phase2(Base, Matrix, 5),  
  maplist(writeln, Matrix),
  statistics(runtime, [_,T]),
  write('CPU time = '), write(T), write(' msec'), nl, nl, true. 