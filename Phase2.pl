
phase2(Rows, Result, Size) :-
    equalize(Rows,Result),
    rule1(Result),
    rule2(Result), 
    transpose(Result,Transposed),
    rule1(Transposed),
    rule2(Transposed),
    rule3(Result).



% Rule 1 - Checks if there are duplicates in a row or column
rule1(M) :- rule1Check(M).

rule1Check([]).
rule1Check([H|T]) :- checkLine2(H), rule1Check(T).

checkLine2(L) :- checkLine2(L,L).
checkLine2(_,[]).
checkLine2(L, [H|T]) :- H = 0, checkLine2(L,T).
checkLine2(L, [H|T]) :- H \= 0, noDuplicates(L,H), checkLine2(L,T).


noDuplicates(L,V) :- noDuplicates(L,V,0). 
noDuplicates([],_,1).
noDuplicates([],_,0).
noDuplicates([V|T], X, R) :- V=X, A is R+1, A<2, noDuplicates(T,X,A).
noDuplicates([V|T], X, R) :- V\=X , noDuplicates(T,X,R).


% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2(M) :- rule2Check(M).

rule2Check([]).
rule2Check([H|T]) :- checkLine(H), rule2Check(T).

checkLine([]) :- true.
checkLine([_]) :- true.
checkLine([I1,I2|T]) :- checkLine([I2|T]), not(blackPair(I1,I2)). 

blackPair(I1,I2) :- I1 == 0, I2 == 0.

% Rule 3
rule3(M) :-
    prepareForFlood(M,PM), 
    firstNonBlack(PM,Start), write(Start), nl,
    flood(PM,Start,[],R),
    flatten(M,FM), 
    nonBlacks(FM,NonBlacks),
    same_length(R, NonBlacks), write('Finished'), nl, write(M), !.

flood(Matrix,[V,X,Y],Visited,Visited) :- V=0.
flood(Matrix,[V,X,Y],Visited,Visited) :- member([V,X,Y], Visited).
flood(Matrix,[V,X,Y],Visited,R0) :-
    V \=0, append(Visited,[[V,X,Y]], R1),
    Xleft is X-1, elementAt(Matrix,Xleft,Y,Eleft),
    Xright is X+1, elementAt(Matrix,Xright,Y,Eright),
    Yup is Y-1, elementAt(Matrix,X,Yup,Eup),
    Ydown is Y+1, elementAt(Matrix,X,Ydown,Edown),
    flood(Matrix,Eleft,R1,R2),
    flood(Matrix,Eright,R2,R3),
    flood(Matrix,Edown,R3,R4),
    flood(Matrix,Eup,R4,R5),
    list_to_set(R5,R0).

    
    

firstNonBlack([H|_],V) :- exclude(ib,H, [V|_]).
ib([V,X,Y]) :- V==0.

nonBlacks(L, Result) :- exclude(isBlack, L, Result).
isBlack(X) :- X == 0.


elementAt(M,X,Y,E) :- nth0(Y, M, Row), nth0(X,Row,E).
elementAt(M,X,Y,[0,0,0]).

prepareForFlood(A,B) :- prepareForFlood(A,B,0).
prepareForFlood([],[],_).
prepareForFlood([H|T], [A|B],Y) :- prepareRow(H,A,0,Y), Y2 is Y+1, prepareForFlood(T,B,Y2).

prepareRow([],[],_,_).
prepareRow([V|T1],[[V,Y,X]|T2],Y,X) :- X2 is X+1, prepareRow(T1,T2,X2,Y).



equalize(BM, SM) :- 
    flatten(BM, FBM), flatten(SM, FSM), equality(FBM,FSM).

equality(A,B) :- equality(A,B,[],[]).
equality([],[],_,_).
equality([H1|T1], [H2|T2], A, B) :- eq(A,B,[H1|T1],[H2|T2]), append(A,[H1],A2), append(B,[H2],B2), equality(T1,T2,A2,B2).



eq(H1,H2,[E1|T1],[E2|T2]) :- E2=0,
    append(H1,[E1|T1],A1), append(H2,[E2|T2],A2), 
    list2matrix(A1,5,R1), list2matrix(A2,5,R2),
    standardCycle(R1,R2).

eq(H1,H2,[E1|T1],[E2|T2]) :- E2=E1,
    append(H1,[E1|T1],A1), append(H2,[E2|T2],A2), 
    list2matrix(A1,5,R1), list2matrix(A2,5,R2),
    standardCycle(R1,R2).

/*
% Equality rule
equality([],[]).
equality([H1|T1], [H2|T2]) :- eq(H1,H2), equality(T1,T2).

eq([],[]).
eq([H1|T1], [H2|T2]) :- H1=H2, eq(T1,T2).
eq([H1|T1], [H2|T2]) :- H2=0, eq(T1,T2).
*/


standardCycle(M,S) :- 
    rotateMatrix(M, M1), rotateMatrix(S,S1), patternSCBlack(M1,S1),
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), patternSCBlack(M2,S2), 
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), patternSCBlack(M3,S3), 
    rotateMatrix(M3, M4), rotateMatrix(S3,S4),  patternSCBlack(M4,S4).

patternSCWhite([],[]).
patternSCWhite([E1|T],[A1|B]) :- checkSCWhite(E1,A1,E1,A1,0), patternSCWhite(T,B).

checkSCWhite([],[],_,_,_).
checkSCWhite([E1|T1], [A1|B1], Base1, Base2, I) :- integer(A1), A1>0, searchSC(Base1,Base2,A1,I), I2 is I+1, checkSCWhite(T1,B1,Base1,Base2,I2).
checkSCWhite([E1|T1], [A1|B1], Base1, Base2, I) :-  I2 is I+1, checkSCWhite(T1,B1,Base1,Base2,I2).

searchSC(A,B,C,D) :- searchSC(A,B,C,D,0).
searchSC([],[],_,_,_).
searchSC([X|T], [0|B], X, I, C) :- I\=C, C2 is C+1, searchSC(T,B,X,I,C2).
searchSC([_|T], [_|B], X, I, C) :- C2 is C+1, searchSC(T,B,X,I,C2).


patternSCBlack([],[]).
patternSCBlack([E1|T],[A1|B]) :- checkSCBlack(E1,A1), patternSCBlack(T,B).

checkSCBlack([_],[_]).
checkSCBlack([E1,E2|T1], [A1,A2|B1]) :- integer(A1), A1=0, A2=E2, checkSCBlack([E2|T1], [A2|B1]).
checkSCBlack([E1,E2|T1], [A1,A2|B1]) :- integer(A1), A1\=0, checkSCBlack([E2|T1], [A2|B1]).
checkSCBlack([E1,E2|T1], [A1,A2|B1]) :- not(integer(A1)), checkSCBlack([E2|T1], [A2|B1]).



length_(Length, List) :- length(List, Length).
list2matrix(List, RowSize, Matrix) :-
    length(List, L),
    HowManyRows is L div RowSize,
    length(Matrix, HowManyRows),
    maplist(length_(RowSize), Matrix),
    append(Matrix, List).