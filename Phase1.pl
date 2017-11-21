

phase1(Rows, Result, Size) :-
    patternSandwich(Rows, Result),
    patternQuadCorner(Rows,Result),
    patternTripleCorner(Rows, Result),
    patternDoubleCorner(Rows, Result),
    patternStandardCycle(Rows, Result).



patternStandardCycle(M, EM) :- patternSCWhite(M, EM), transpose(M,T), transpose(EM,TEM), patternSCWhite(T,TEM).   

patternSCWhite([],[]).
patternSCWhite([E1|T],[A1|B]) :- checkSCWhite(E1,A1,E1,A1), patternSCWhite(T,B).

checkSCWhite([],[],_,_).
checkSCWhite([E1|T1], [A1|B1], Base1, Base2) :- integer(A1), searchSC(Base1,Base2,A1), checkSCWhite(T1,B1,Base1,Base2).
checkSCWhite([E1|T1], [A1|B1], Base1, Base2) :- checkSCWhite(T1,B1,Base1,Base2).

searchSC([],[],_).
searchSC([X|T], [0|B], X) :- searchSC(T,B,X).
searchSC([_|T], [_|B], X) :- searchSC(T,B,X).


patternSCBlack([],[]).
patternSCBlack([E1|T],[A1|B]) :- checkSCBlack(E1,A1,A1), patternSCBlack(T,B).

checkSCBlack([],[],[]).
checkSCBlack([E1,E2|T1], [0,_|B1], [_,E2|B1]) :- integer(A1),  checkSCBlack(T1,B1,Base1,Base2).
checkSCBlack([_,_|T1], [_,_|B1],[_,_|B2]) :- checkSCBlack(T1,B1,B2).

/*
patternSC([],[]).
patternSC([E1|T],[A1|B]) :- checkSC(E1,A1,A1), patternSC(T,B).

checkSC([_],[_],[_]).
checkSC([E1,E2|T1], [0,_|B1], [_,E2|B2]) :- checkSC([_|T1],[_|B1],[_|B2]).
checkSC([_,_|T1], [_,_|B1], [_,_|B2]) :- checkSC([_|T1],[_|B1],[_|B2]).
*/


% Pattern sandwich. This does both the sanwich triple and the sandwich pair!
patternSandwich(M, EM) :- extractLineP1(M, EM), transpose(M,T), transpose(EM,TEM), extractLineP1(T,TEM).

extractLineP1([],[]).
extractLineP1([H|T],[A|B]) :- checkLineP1(H,A), extractLineP1(T,B).

checkLineP1([_,_],[_,_]).
checkLineP1([E1,E2,E1|T], [_,E2,A3|B]) :- checkLineP1([E2,_|T],[_,A3|B]).
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
checkDC([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]) :- E1\=E2, E1\=E3, E3\=E4, E2\=E4.


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