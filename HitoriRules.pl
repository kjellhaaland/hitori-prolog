:- use_module(library(clpfd)).
test :- write(' hellu').

/* What do we need?

- Rule 1    NO NEED FOR THIS?
- Rule 2    DONE
- Rule 3    DONE

- SetBlack
- SetWhite
- SetNeigboursWhite
- SetDuplicatesBlack

- Phase3

*/

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


/*  Replaces a element in a matrix
    M is the matrix
    X is the row
    Y is the column
    V is the new value at the given position
    R is the new matrix
*/
replaceMatrix(M,X,Y,V,Result) :- row(M,X,R1), replace(R1,Y,V,R2), replace(M,X,R2,Result). 
replace(L, P, E, R) :- findall(X, (nth1(I,L,Y), (I == P -> X=E ; X=Y)), R).


% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2(M) :- rule2Check(M), transpose(M,T), rule2Check(T).

rule2Check([]).
rule2Check([H|T]) :- checkLine(H), rule2Check(T).

checkLine([]) :- true.
checkLine([_]) :- true.
checkLine([I1,I2|T]) :- checkLine([I2|T]), not(blackPair(I1,I2)). 

blackPair(I1,I2) :- I1 == 'B', I2 == 'B'.
