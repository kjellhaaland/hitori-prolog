:- use_module(library(clpfd)).

/* What do we need?

- Rule 1    NO NEED FOR THIS?
- Rule 2    DONE
- Rule 3    TODO

- SetBlack              DONE
- SetWhite              ALMOST DONE
- SetNeigboursWhite     DONE
- SetDuplicatesBlack    TODO

- Phase3                TODO

*/



test(L,X,Y,R) :- test(L,E,X,Y,R).
test([],Items,_,_,R) :- R is Items.
test([H|T],Items, X, Y, R) :- A is [X,Y,H], append(A,Items, R2), X2 is X+1, test(T,R2,X2,Y,R).


/*
unsolved(M,Indexes) :- unsolved(M,1,1,Xs,Ys).
unsolved(M,X,Y,Xs,Ys) :- row(M,X,R), unsolvedSearch(R,X,1,Xs,Ys), X2 is X+1, unsolved(M,X2,Y,Xs,Ys). 
*/
/*

unsolvedSearch([H|T], X, Y, Xs, Ys) :- (H = 'W' ; H = 'B'), Y2 is Y+1, unsolvedSearch(T,X,Y2,Xs,Ys).
*/
unsolvedSearch([], X, Y, A, B, Xs, Ys) :- Xs is A, Ys is B.
unsolvedSearch([H|T], X, Y, A, B, Xs, Ys) :- H \= 'W', H \= 'B', Y2 is Y+1, unsolvedSearch(T, X, Y2, [X|Xs], [Y|Ys], Xs, Ys).






% Gets the row of the board(B) given by the index(I)
row(B,I,R) :- nth1(I, B, R).

% Gets the col of the board(B) given by the index(I)
col(B,I,C) :- transpose(B, T), row(T,I,C).


% Paints the cell at the given X and Y position black
paintBlack(M,X,Y,Result) :- replaceMatrix(M,X,Y,'B',R1), 
YRight is Y+1, paintWhite(R1,X,YRight,R2),
YLeft is Y-1, paintWhite(R2,X,YLeft,R3),
XTop is X+1, paintWhite(R3,XTop,Y,R4),
XBottom is X+1, paintWhite(R4,XBottom,Y,Result).

% Paints the cell at the given X and y position white
paintWhite(M,X,Y,Result) :- replaceMatrix(M,X,Y,'W',Result).


/*
valueAt(M,X,Y,V),
row(M,X,R)
*/

% Value at
valueAt(M,X,Y,V) :- row(M,X,R) , nth1(Y,R,V).


/*  Replaces a element in a matrix
    M is the matrix
    X is the row
    Y is the column
    V is the new value at the given position
    R is the new matrix
*/
replaceMatrix(M,X,Y,V,Result) :- row(M,X,R1), replace(R1,Y,V,R2), replace(M,X,R2,Result). 
replace(L, P, E, R) :- findall(X, (nth1(I,L,Y), (I == P -> X=E ; X=Y)), R).



% Rule 1 - Checks if there are duplicates in a row or column
rule1(M) :- rule1Check(M), transpose(M,T), rule1Check(T).

rule1Check([]).
rule1Check([H|T]) :- checkLine2(H), rule1Check(T).

checkLine2(L) :- checkLine2(L,L).
checkLine2(_,[]).
checkLine2(L, [H|T]) :- (H = 'W' ; H = 'B'), checkLine2(L,T).
checkLine2(L, [H|T]) :- H \= 'W', H \= 'B', count(L,H,R), R=1, checkLine2(L,T).

% Counts the occuriencies of the element X in the list.
count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).



% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2(M) :- rule2Check(M), transpose(M,T), rule2Check(T).

rule2Check([]).
rule2Check([H|T]) :- checkLine(H), rule2Check(T).

checkLine([]) :- true.
checkLine([_]) :- true.
checkLine([I1,I2|T]) :- checkLine([I2|T]), not(blackPair(I1,I2)). 

blackPair(I1,I2) :- I1 == 'B', I2 == 'B'.
