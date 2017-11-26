
phase2(Rows, Result, Size) :-
    equality(Rows,Result),
    rule1(Result),
    rule2(Result), 
    transpose(Result,Transposed),
    rule1(Transposed),
    rule2(Transposed).


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


% Equality rule
equality([],[]).
equality([H1|T1], [H2|T2]) :- eq(H1,H2), equality(T1,T2).

eq([],[]).
eq([H1|T1], [H2|T2]) :- H1=H2, eq(T1,T2).
eq([H1|T1], [H2|T2]) :- H2=0, eq(T1,T2).


standardCycle(M,S) :- 
    rotateMatrix(M, M1), rotateMatrix(S,S1), patternSCWhite(M1,S1), patternSCBlack(M1,S1),
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), patternSCWhite(M2,S2), patternSCBlack(M2,S2), 
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), patternSCWhite(M3,S3), patternSCBlack(M3,S3), 
    rotateMatrix(M3, M4), rotateMatrix(S3,S4), patternSCWhite(M4,S4), patternSCBlack(M4,S4).

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
patternSCBlack([E1|T],[A1|B]) :- checkSCBlack(E1,A1,E1,A1), patternSCBlack(T,B).

checkSCBlack([_],[_],_,_).
checkSCBlack([E1,E2|T1], [A1,A2|B1], [E3,E4|T2], [_,E2|B2]) :- integer(A1), A1=0,checkSCBlack([E2|T1], [A2|B1], [E4|T2], [E2|B2]).
checkSCBlack([_,E2|T1], [_,A2|B1], [_,E4|T1], [_,A4|B2]) :- checkSCBlack([E2|T1], [A2|B1], [E4|T2], [A4|B2]).


