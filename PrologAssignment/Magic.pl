% Rotates the given matix to the right
rotateMatrix(M, Rotated) :-
    transpose(M, X),
    maplist(reverse, X, Rotated).

% Gets the row of the board(B) given by the index(I)
row(B,I,R) :- nth0(I, B, R).

% Gets the col of the board(B) given by the index(I)
col(B,I,C) :- transpose(B, T), row(T,I,C).

% Performs the chain reactions algorithm until there is no changes!
doChainReactions(M,S) :- doChainReactions(M,S,99999,0).         % Starts with a high number to secure at least one cycle
doChainReactions(M, S, CBefore, CAfter) :- 
    CAfter<CBefore,
    countUnsolvedInMatrix(S,C1),
    chainReactions(M,S),!,
    countUnsolvedInMatrix(S,C2),
    prep(M,S,M2),
    doChainReactions(M2,S,C1,C2),!.

doChainReactions(M,S,CBefore,CAfter) :- CAfter>=CBefore.

/*
 * Chain reactions paint cells based on the two first rules of hitori.
 * Matrix is rotated for each time the black pattern is checked
 */
chainReactions(M,S) :-
    rotateMatrix(M, M1), rotateMatrix(S,S1), 
    patternStandardCycleBlack(M1,S1),!,
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), 
    patternStandardCycleBlack(M2,S2),!, 
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), 
    patternStandardCycleBlack(M3,S3),!, 
    rotateMatrix(M3, M4), rotateMatrix(S3,S4),
    patternStandardCycleBlack(M4,S4),
    patternStandardCycleWhite(M4,S4),!,
    patternUnique(M4,S4).
    
% Sets all duplicates in a white cells row and column as black
patternStandardCycleWhite(BM,SM) :- patternStandardCycleWhite(BM,SM,BM,SM,0).
patternStandardCycleWhite(_,_,[],[],_).
patternStandardCycleWhite(BM,SM,[E1|T],[A1|B],Y) :- checkStandardCycleWhite(BM,SM,E1,A1,0,Y), Y2 is Y+1, patternStandardCycleWhite(BM,SM,T,B,Y2).

checkStandardCycleWhite(_,_,[],[],_,_).
checkStandardCycleWhite(BM,SM,[_|T],[A|B],X,Y) :- isUnknown(A), !, X2 is X+1, checkStandardCycleWhite(BM,SM,T,B,X2,Y).
checkStandardCycleWhite(BM,SM,[H|T],[A|B],X,Y) :- isSolved(H),!, X2 is X+1, checkStandardCycleWhite(BM,SM,T,B,X2,Y).
checkStandardCycleWhite(BM,SM,[_|T],[A|B],X,Y) :- A=0,!, X2 is X+1, checkStandardCycleWhite(BM,SM,T,B,X2,Y).
checkStandardCycleWhite(BM,SM,[H|T],[A|B],X,Y) :- A=H, 
    row(BM,Y,BMRow), row(SM,Y,SMRow), searchSCWhite(BMRow,SMRow,[A,X]),
    col(BM,X,BMCol), col(SM,X,SMCol), searchSCWhite(BMCol,SMCol,[A,Y]), 
    X2 is X+1, checkStandardCycleWhite(BM,SM,T,B,X2,Y).

searchSCWhite(BL,SL,I) :- searchSCWhite(BL,SL,I,0).
searchSCWhite([],[],_,_).
searchSCWhite([H|T],[A|B],[V,I],I2) :- isSolved(H),!, I3 is I2+1, searchSCWhite(T,B,[V,I],I3).
searchSCWhite([H|T],[A|B],[V,I],I2) :- H=V, I2\=I, A=0,!, I3 is I2+1, searchSCWhite(T,B,[V,I],I3).
searchSCWhite([H|T],[A|B],[V,I],I2) :- H=V, I2=I,!,I3 is I2+1, searchSCWhite(T,B,[V,I],I3).
searchSCWhite([H|T],[_|B],[V,I],I2) :- H\=V,!, I3 is I2+1, searchSCWhite(T,B,[V,I],I3).

% Sets the left neighbour to a cell as white if the cell itself is painted black
patternStandardCycleBlack([],[]).
patternStandardCycleBlack([E1|T],[A1|B]) :- checkStandardCycleBlack(E1,A1), patternStandardCycleBlack(T,B).

checkStandardCycleBlack([_],[_]).
checkStandardCycleBlack([E1,E2|T1], [A1,A2|B1]) :- ( isSolved(E1) ; isSolved(E2) ; isUnknown(A1) ), !, checkStandardCycleBlack([E2|T1], [A2|B1]).
checkStandardCycleBlack([E1,E2|T1], [A1,A2|B1]) :- A1=0, A2=E2, !, checkStandardCycleBlack([E2|T1], [A2|B1]).
checkStandardCycleBlack([E1,E2|T1], [A1,A2|B1]) :- A1\=0, !, checkStandardCycleBlack([E2|T1], [A2|B1]).

% Defines if the cell is solved 
isSolved(V) :- V=0.

% Defines if the cell is unknown (unbound)
isUnknown(V) :- integer(V), !, false.
isUnknown(_).