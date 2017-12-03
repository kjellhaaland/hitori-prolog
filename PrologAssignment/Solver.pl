
:- use_module(library(clpfd)).

% Include the phases
:- include('Phase2').
:- include('Phase1').
:- include('Magic').


% Creates an empty matrix with the same dimensions as the given matrix
createBlankMatrix([],[]).
createBlankMatrix([H|T], [A|B]) :- createEmptyRow(H,A), createBlankMatrix(T,B).

createEmptyRow([],[]).
createEmptyRow([_|T1],[_|T2]) :- createEmptyRow(T1,T2).


% Swaps all original values of solved items (bound items) in the BaseMatrix with 0.
prep(M1,M2,M3) :- prepareMatrix(M1,M2,M3),!.

prepareMatrix([],[],[]).
prepareMatrix([H|T], [A|B], [C|D]) :- prepareRow(H,A,C),!, prepareMatrix(T,B,D).

prepareRow([],[],[]).
prepareRow([_|T1],[B|T2],[0|T3]) :- integer(B), !, prepareRow(T1,T2,T3).
prepareRow([A|T1],[_|T2],[A|T3]) :- prepareRow(T1,T2,T3).


% Copys the board to a new matrix
mapMatrix([],[]).
mapMatrix([H|T], [A|B]) :- mapRow(H,A), mapMatrix(T,B).

mapRow([],[]).
mapRow([H|T1],['X'|T2]) :- integer(H), H=0, mapRow(T1,T2).
mapRow([H|T1],[H|T2]) :- integer(H), mapRow(T1,T2).

% For testing
mapRow([_|T1],['U'|T2]) :- mapRow(T1,T2).

/*
 * Main predicate of control
 * Solves the given matrix by using tho phases1
 * Phase1 is based on patterns and chain reactions
 * Phase2 is based on brute-force combined with chain reactions
 */ 
solveMatrix(Size, _, BaseMatrix, ResultMatrix) :-
    statistics(runtime, _),    
    createBlankMatrix(BaseMatrix,SolutionMatrix),          % Creates a blank matrix with the same dimensions as the BaseMatrix
    phase1(BaseMatrix, SolutionMatrix, Size),              % Fills some of the cells in the SolutionMatrix based on known patterns
    prep(BaseMatrix, SolutionMatrix, PreparedMatrix),!,    % Prepares the matrix for phase2 by replacing all solved cells with 0. (They does not need to be checked again because they are already solved)
    phase2(PreparedMatrix, SolutionMatrix, Size),          % Fills the remaining unbound cells by brute-force and chain reactions. Will fail if if no solution is found.
    statistics(runtime, [_,TA]), nl,
    write('CPU time = '), write(TA), write(' msec'), nl,    
    mapMatrix(SolutionMatrix, ResultMatrix).               % Copy the solution to the resulting matrix (So that it is in a printable format)