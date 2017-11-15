
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


% Copys the board to a new matrix
mapMatrix([],[]).
mapMatrix([H|T], [A|B]) :- mapRow(H,A), mapMatrix(T,B).

mapRow([],[]).
mapRow([0|T1],['X'|T2]) :- mapRow(T1,T2).
mapRow([H|T1],[H|T2]) :- mapRow(T1,T2).

% Solves the given puzzle with two phases
solveMatrix(Size, _, Matrix, Result) :-
    statistics(runtime, _),
    createBlankMatrix(Matrix,BlankMatrix),
    phase1(Matrix, BlankMatrix, Size),
    phase2(Matrix,BlankMatrix, Size),
    statistics(runtime, [_,T]),
    write('CPU time = '), write(T), write(' msec'), nl, nl,
    mapMatrix(BlankMatrix, Result). 