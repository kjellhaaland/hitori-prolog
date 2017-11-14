

phase1(Rows, Result, Size) :-
    equalityph1(Row, Result),
    patternSandwich(Rows, Result), 
    patternDoubleCorner(Rows,Result).



% Equality rule for patterns
equalityph1([],[]).
equalityph1([H1|T1], [H2|T2]) :- eqph1(H1,H2), equalityph1(T1,T2).

eqph1([],[]).
eqph1([H1|T1], [H2|T2]) :- (H1=H2  ; H2=0 ; H2=_), eqph1(T1,T2).


% Pattern sandwich. This does both the sanwich triple and the sandwich pair!
patternSandwich(M, EM) :- extractLineP1(M, EM), transpose(M,T), transpose(EM,TEM), extractLineP1(T,TEM).

extractLineP1([],[]).
extractLineP1([H|T],[A|B]) :- checkLineP1(H,A), extractLineP1(T,B).

checkLineP1([_,_],[_,_]).
checkLineP1([E1,E2,E1|T], [A1,E2,A3|B]) :- checkLineP1([E2,E3|T],[A2,A3|B]).
checkLineP1([E1,E1,E1|T], [0,E1,0|B]) :- checkLineP1([E2,E3|T],[A2,A3|B]).
checkLineP1([E1,E2,E3|T], [A1,A2,A3|B]) :- (E1\=E3; E1\=E2 ; E1\=E3), checkLineP1([E2,E3|T],[A2,A3|B]).




patternDoubleCorner(M, S) :- 
    doSomething(M,S),
    rotateMatrix(M, M1), rotateMatrix(S,S1), doSomething(M1,S1),
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), doSomething(M2,S2),
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), doSomething(M3,S3).

doSomething([E1,E2|T], [A1,A2|B]) :- top(E1,E2,A1,A2).

/*
top([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]) :- E1=E2, A3=E3.
top([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]) :- E1=E3, A2=E2.
top([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]) :- E3=E4, A2=E2.
top([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]) :- E2=E4, A3=E3.
top([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]) :- E1\=E2, E1\=E3, E3\=E4, E2\=E4.
*/

top([E1,E1|_], [E3,E4|_], [A1,A2|_], [E3,A4|_]).
top([E1,E2|_], [E1,E4|_], [A1,E2|_], [A3,A4|_]).
top([E1,E2|_], [E3,E3|_], [A1,E2|_], [A3,A4|_]).
top([E1,E2|_], [E3,E2|_], [A1,A2|_], [E3,A4|_]).
top([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]) :- E1\=E2, E1\=E3, E3\=E4, E2\=E4.

/*
extractLineP2(M,A) :- extractLineP2(M,A,1).
extractLineP2([_],[_],_).
extractLineP2([R1,R2|_],[A1,A2|_],I) :- I2 is I+1, extractLineP2(T,B, I2).
extractLineP2([R1,R2|T],[A1,A2|B],I) :- I=1, checkLineP2(R1,R2,A1,A2), I2 is I+1, extractLineP2(T,B, I2).
extractLineP2([R1,R2|T],[A1,A2|B],I) :- I>1, checkLineP2(R1,R2,A1,A2), I2 is I+1, extractLineP2(T,B, I2).

checkLineP2(R1,R2,A1,A2) :- checkLineP2(R1,R2,A1,A2,1).
checkLineP2([_,_],[_,_],[_,_],[_,_]).
checkLineP2([E1,E1|T1], [E3,E4|T2], [A1,A2|B1], [E3,A4|B1], I) :- I=1, I2 is I+1, checkLineP2(T1,T2,B1,B2,I2).
checkLineP2([E1,E2|T1], [E3,E4|T2], [A1,A2|B1], [A3,A4|B1], I) :- I=1, E1\=E2, I2 is I+1, checkLineP2(T1,T2,B1,B2,I2).
checkLineP2([E1,E2|T1], [E3,E4|T2], [A1,A2|B1], [A3,A4|B1], I) :- I2 is I+1, checkLineP2(T1,T2,B1,B2,I2).
checkLineP2([E1,E2|_], [E3,E3|_], [E1,A2|_], [A3,A4|_], I) :- I2 is I+1, checkLineP2(T1,T2,B1,B2,I2).
*/


% Problem solving things...
solvedProblem([[0,2,5,4,3],[4,5,0,1,0],[1,0,3,0,4],[3,4,1,2,5],[0,1,4,0,2]]).
hProblem([
    [1,1,5,4,3],
    [4,5,4,1,1],
    [1,1,3,1,4],
    [3,4,1,2,5],
    [3,1,4,1,2]]).

blank([[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_]]).

test_phase1 :-
    statistics(runtime, _),
    hProblem(Rows),
    blank(Blank),
    phase1(Rows,Blank,5),  
    maplist(writeln, Blank),
    statistics(runtime, [_,T]),
    write('CPU time = '), write(T), write(' msec'), nl, nl, true. 
