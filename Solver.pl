
:- use_module(library(clpfd)).

% Include the phases
:- include('Phase2.pl').
:- include('Phase1.pl').
:- include('Common.pl').


% Creates an empty matrix with the same dimensions as the given matrix
createBlankMatrix([],[]).
createBlankMatrix([H|T], [A|B]) :- createEmptyRow(H,A), createBlankMatrix(T,B).

createEmptyRow([],[]).
createEmptyRow([_|T1],[_|T2]) :- createEmptyRow(T1,T2).

% Creates an empty matrix with the same dimensions as the given matrix

prep(M1,M2,M3) :- prepareMatrix(M1,M2,M3),!.

prepareMatrix([],[],[]).
prepareMatrix([H|T], [A|B], [C|D]) :- prepareRow(H,A,C),!, prepareMatrix(T,B,D).

prepareRow([],[],[]).
prepareRow([A|T1],[B|T2],[0|T3]) :- integer(B), !, prepareRow(T1,T2,T3).
prepareRow([A|T1],[B|T2],[A|T3]) :- prepareRow(T1,T2,T3).


% Copys the board to a new matrix
mapMatrix([],[]).
mapMatrix([H|T], [A|B]) :- mapRow(H,A), mapMatrix(T,B).

mapRow([],[]).
mapRow([H|T1],['X'|T2]) :- integer(H), H=0, mapRow(T1,T2).
mapRow([H|T1],[H|T2]) :- integer(H), mapRow(T1,T2).
% For testing
mapRow([H|T1],['U'|T2]) :- mapRow(T1,T2).

% Solves the given puzzle with two phases
solveMatrix(Size, _, Matrix, Result) :-
    statistics(runtime, _),
    createBlankMatrix(Matrix,BlankMatrix),
    phase1(Matrix, BlankMatrix, Size),
    prep(Matrix, BlankMatrix, PreparedMatrix),
    phase2(PreparedMatrix, BlankMatrix, Size),
    statistics(runtime, [_,T]),
    write('CPU time = '), write(T), write(' msec'), nl, nl,
    mapMatrix(BlankMatrix, Result). 