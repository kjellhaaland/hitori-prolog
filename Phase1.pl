

phase1(Rows, Result, Size) :-
    patternSandwich(Rows, Result), 
    patternDoubleCorner(Rows,Result).

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
