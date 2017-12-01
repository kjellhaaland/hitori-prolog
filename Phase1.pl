

phase1(Rows, Result, Size) :-
    patternSandwich(Rows, Result),
    patternQuadCorner(Rows,Result),
    patternTripleCorner(Rows, Result),
    patternDoubleCorner(Rows, Result),
    patternStandardCycle(Rows, Result)
    , patternUnique(Rows,Result)
    .


patternUnique(BM,SM) :- patternUnique(BM,SM,BM,SM,0).
patternUnique(_,_,[],[],_).
patternUnique(BM,SM,[H1|T],[A1|B],Y) :- uniqueness(BM,SM,H1,A1,0,Y), Y2 is Y+1, patternUnique(BM,SM,T,B,Y2).

uniqueness(_,_,[],[],_,_).
uniqueness(BM,SM, [H|T], [A|B], X, Y) :- isUnknown(A), 
    row(BM,Y,BMRow), row(SM,Y,SMRow), countUnsolvedDuplicates(BMRow, SMRow,H,CRow), CRow=1,
    col(BM,X,BMCol), col(SM,X,SMCol), countUnsolvedDuplicates(BMCol, SMCol,H,CCol), CCol=1,!, A=H,
    X2 is X+1, uniqueness(BM,SM,T,B,X2,Y).

uniqueness(BM,SM, [_|T], [_|B], X, Y) :- X2 is X+1, uniqueness(BM,SM,T,B,X2,Y).


patternStandardCycle(M,S) :- countUnsolvedInMatrix(S,C1), patternStandardCycle(M,S,C1,999999).
patternStandardCycle(M,S,A,A).
patternStandardCycle(M, S, C1, C2) :- C1\=C2, standardCycle(M,S),
    countUnsolvedInMatrix(S,C3), patternStandardCycle(M,S,C2,C3).


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


% Pattern sandwich. This does both the sanwich triple and the sandwich pair!
patternSandwich(M, EM) :- extractLineP1(M, EM), transpose(M,T), transpose(EM,TEM), extractLineP1(T,TEM).

extractLineP1([],[]).
extractLineP1([H|T],[A|B]) :- checkLineP1(H,A), extractLineP1(T,B).

checkLineP1([_,_],[_,_]).
checkLineP1([E1,E2,E1|T], [_,E2,A3|B]) :- checkLineP1([E2,E1|T],[E2,A3|B]).
checkLineP1([E1,E1,E1|T], [0,E1,0|B]) :- checkLineP1([E2,E3|T],[A2,A3|B]).
checkLineP1([E1,E2,E3|T], [A1,A2,A3|B]) :- (E1\=E3; E1\=E2 ; E1\=E3), checkLineP1([E2,E3|T],[A2,A3|B]).


% Pattern double corner
patternDoubleCorner(M, S) :- 
    rotateMatrix(M, M1), rotateMatrix(S,S1), patternDC(M1,S1),
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), patternDC(M2,S2),
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), patternDC(M3,S3),
    rotateMatrix(M3, M4), rotateMatrix(S3,S4), patternDC(M4,S4).

patternDC([E1,E2|T], [A1,A2|B]) :- checkDC(E1,E2,A1,A2).

checkDC([E1,E1|_], [E3,E4|_], [A1,A2|_], [E3,A4|_]).
checkDC([E1,E2|_], [E1,E4|_], [A1,E2|_], [A3,A4|_]).
checkDC([E1,E2|_], [E3,E3|_], [A1,E2|_], [A3,A4|_]).
checkDC([E1,E2|_], [E3,E2|_], [A1,A2|_], [E3,A4|_]).
checkDC([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]).


% Pattern triple corner
patternTripleCorner(M, S) :- 
    rotateMatrix(M, M1), rotateMatrix(S,S1), patternTC(M1,S1),
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), patternTC(M2,S2),
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), patternTC(M3,S3),
    rotateMatrix(M3, M4), rotateMatrix(S3,S4), patternTC(M4,S4).

patternTC([E1,E2|T], [A1,A2|B]) :- checkTC(E1,E2,A1,A2).

checkTC([E1,E1|_], [E1,_|_], [0,E1|_], [E1,_|_]). % Three eq. num. in a corner.
checkTC([E1,E2|_], [E2,E2|_], [_,E2|_], [E2,0|_]). % Flipped triplecorner
checkTC([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]).


% Pattern quad corner
patternQuadCorner(M, S) :-
    rotateMatrix(M, M1), rotateMatrix(S,S1), patternQC(M1,S1),
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), patternQC(M2,S2),
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), patternQC(M3,S3),
    rotateMatrix(M3, M4), rotateMatrix(S3,S4), patternQC(M4,S4).

patternQC([E1,E2|T], [A1,A2|B]) :- checkQC(E1,E2,A1,A2).

checkQC([E1,E1|_], [E1,E1|_], [0,E1|_], [E1,0|_]). % Four eq. numbers in a corner.
checkQC([E1,E1|_], [E2,E2|_], [0,E1|_], [E2,0|_]). % Two eq. pairs in a corner horizontally.
checkQC([E1,E2|_], [E1,E2|_], [0,E2|_], [E1,0|_]). % Two eq. pairs in a corner vertically.
checkQC([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]).





% Counts the occuriencies of the element X in the list.
count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

% Counts the occuriencies of the element X in the list.
countExcludeBlacks([],_,0).
countExcludeBlacks([X|T],X,Y):- integer(X), countExcludeBlacks(T,X,Z), Y is 1+Z.
countExcludeBlacks([X1|T],X,Z):- X1\=X,countExcludeBlacks(T,X,Z).

% Counts the occuriencies of the element X in the list.
countUnsolvedInMatrix(M,Y) :- flatten(M, Flat), countUnsolved(Flat,Y).

countUnsolved([],0).
countUnsolved([X|T], Y):- isUnknown(X), !,countUnsolved(T,Z), Y is 1+Z.
countUnsolved([X|T], Y):- countUnsolved(T,Y).

countUnsolvedDuplicates([],[],_,0).
countUnsolvedDuplicates([X|T],[A|B], V, Y):- isUnknown(A), X=V, !, countUnsolvedDuplicates(T,B,V,Z), Y is 1+Z.
countUnsolvedDuplicates([X|T],[A|B], V, Y):- countUnsolvedDuplicates(T,B,V,Y).

%[ [1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9] ]