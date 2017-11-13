:- use_module(library(clpfd)).


phase3(Rows, Result, Size) :-
    append(Result, L),
    L ins (0..Size),
    equality(Rows,Result),
    rule1(Result),
    rule2(Result),
    maplist(label, Result).    

% Rule 1 - Checks if there are duplicates in a row or column
rule1(M) :- rule1Check(M), transpose(M,T), rule1Check(T).

rule1Check([]).
rule1Check([H|T]) :- checkLine2(H), rule1Check(T).

checkLine2(L) :- checkLine2(L,L).
checkLine2(_,[]).
checkLine2(L, [H|T]) :- H = 0, checkLine2(L,T).
checkLine2(L, [H|T]) :- H \= 0, count(L,H,R), R=1, checkLine2(L,T).



% Counts the occuriencies of the element X in the list.
count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).


% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2(M) :- rule2Check(M), transpose(M,T), rule2Check(T).

rule2Check([]).
rule2Check([H|T]) :- checkLine(H), rule2Check(T).

checkLine([]) :- true.
checkLine([_]) :- true.
checkLine([I1,I2|T]) :- checkLine([I2|T]), not(blackPair(I1,I2)). 

blackPair(I1,I2) :- I1 == 0, I2 == 0.



% Rule 3 - All non-black cells must be connected

% Equality rule
equality([],[]).
equality([H1|T1], [H2|T2]) :- eq(H1,H2), equality(T1,T2).

eq([],[]).
eq([H1|T1], [H2|T2]) :- (H1=H2  ; H2=0 ), eq(T1,T2).



% Problem solving things...
solvedProblem([[0,2,5,4,3],[4,5,0,1,0],[1,0,3,0,4],[3,4,1,2,5],[0,1,4,0,2]]).
hProblem([[1,2,5,4,3],[4,5,4,1,1],[1,1,3,1,4],[3,4,1,2,5],[3,1,4,1,2]]).
blank([[_,_,_,_,_],[4,5,_,1,_],[_,_,3,_,_],[3,4,_,2,_],[_,_,4,_,_]]).

solve_problems :-
    statistics(runtime, _),
    hProblem(Rows),
    blank(Blank),
    phase3(Rows,Blank,5),  
    maplist(writeln, Blank),
    statistics(runtime, [_,T]),
    write('CPU time = '), write(T), write(' msec'), nl, nl, true. 
