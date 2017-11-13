:- use_module(library(clpfd)).

phase1(Rows, Result, Size) :-
    equality(Row, Result),
    patternSandwich(Rows, Result).



% Equality rule
equality([],[]).
equality([H1|T1], [H2|T2]) :- eq(H1,H2), equality(T1,T2).

eq([],[]).
eq([H1|T1], [H2|T2]) :- (H1=H2  ; H2=0 ; H2=_), eq(T1,T2).



patternSandwich(M, EM) :- extractLineP1(M, EM), transpose(M,T), transpose(EM,TEM), extractLineP1(T,TEM).

extractLineP1([],[]).
extractLineP1([H|T],[A|B]) :- checkLineP1(H,A), extractLineP1(T,B).

checkLineP1([_,_],[_,_]).
checkLineP1([E1,E2,E1|T], [A1,E2,A3|B]) :- checkLineP1([E2,E3|T],[A2,A3|B]).
checkLineP1([E1,E1,E1|T], [0,E1,0|B]) :- checkLineP1([E2,E3|T],[A2,A3|B]).
checkLineP1([E1,E2,E3|T], [A1,A2,A3|B]) :- (E1\=E3; E1\=E2 ; E1\=E3), checkLineP1([E2,E3|T],[A2,A3|B]).


% Problem solving things...
solvedProblem([[0,2,5,4,3],[4,5,0,1,0],[1,0,3,0,4],[3,4,1,2,5],[0,1,4,0,2]]).
hProblem([[1,2,5,4,3],[4,5,4,1,1],[1,1,3,1,4],[3,4,1,2,5],[3,1,4,1,2]]).
blank([[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_]]).

solve_problems_1 :-
    statistics(runtime, _),
    hProblem(Rows),
    blank(Blank),
    phase1(Rows,Blank,5),  
    maplist(writeln, Blank),
    statistics(runtime, [_,T]),
    write('CPU time = '), write(T), write(' msec'), nl, nl, true. 