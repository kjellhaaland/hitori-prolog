
/*
 * BM = BaseMatrix - the fully filled matrix loaded from the input file
 * SM = SolutionMatrix - the matrix that holds the (eventual) solution
 */

/*
 * Phase 1 searches for patterns in the matrix
 * If the board is a valid board, the board will always be in a valid state after phase1 
 */
phase1(BM, SM, _) :-
    patternSandwich(BM, SM),
    patternQuadCorner(BM, SM),
    patternTripleCorner(BM, SM),
    patternDoubleCorner(BM, SM),
    doChainReactions(BM,SM),
    patternUnique(BM,SM).

/*
 * PatternUnique searches for cells that are unique in its row and column.
 * If they are unique, the cell must be white.
 */
patternUnique(BM,SM) :- patternUnique(BM,SM,BM,SM,0).
patternUnique(_,_,[],[],_). % End at the end of the list
patternUnique(BM,SM,[H1|T],[A1|B],Y) :- uniqueness(BM,SM,H1,A1,0,Y), Y2 is Y+1, patternUnique(BM,SM,T,B,Y2). % Recursively go through the entire board

uniqueness(_,_,[],[],_,_). % Stop at the end of the list
uniqueness(BM,SM, [H|T], [A|B], X, Y) :- isUnknown(A), % Only do the following if the given cell is non-bound
    row(BM,Y,BMRow), row(SM,Y,SMRow), countUnsolvedDuplicates(BMRow, SMRow,H,CRow), CRow=1,
    col(BM,X,BMCol), col(SM,X,SMCol), countUnsolvedDuplicates(BMCol, SMCol,H,CCol), CCol=1,!, A=H,  % If there are no dupliates, A must be H
    X2 is X+1, uniqueness(BM,SM,T,B,X2,Y).  % Recursion

uniqueness(BM,SM, [_|T], [_|B], X, Y) :- X2 is X+1, uniqueness(BM,SM,T,B,X2,Y). % If the cell is bound, reccursion


% Pattern sandwich. This does both the sanwich triple and the sandwich pair!
patternSandwich(M, EM) :- extractLineP1(M, EM), transpose(M,T), transpose(EM,TEM), extractLineP1(T,TEM).

extractLineP1([],[]).
extractLineP1([H|T],[A|B]) :- checkLineP1(H,A), extractLineP1(T,B).

checkLineP1([_,_],[_,_]).
checkLineP1([E1,E2,E1|T], [_,E2,A3|B]) :- checkLineP1([E2,E1|T],[E2,A3|B]).
checkLineP1([E1,E1,E1|T], [0,E1,0|B]) :- checkLineP1([_,_|T],[_,_|B]).
checkLineP1([E1,E2,E3|T], [_,A2,A3|B]) :- (E1\=E3; E1\=E2 ; E1\=E3), checkLineP1([E2,E3|T],[A2,A3|B]).


% Pattern double corner
patternDoubleCorner(M, S) :- 
    rotateMatrix(M, M1), rotateMatrix(S,S1), patternDC(M1,S1),
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), patternDC(M2,S2),
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), patternDC(M3,S3),
    rotateMatrix(M3, M4), rotateMatrix(S3,S4), patternDC(M4,S4).

patternDC([E1,E2|_], [A1,A2|_]) :- checkDC(E1,E2,A1,A2).

checkDC([E1,E1|_], [E3,_|_], [_,_|_], [E3,_|_]).
checkDC([E1,E2|_], [E1,_|_], [_,E2|_], [_,_|_]).
checkDC([_,E2|_], [E3,E3|_], [_,E2|_], [_,_|_]).
checkDC([_,E2|_], [E3,E2|_], [_,_|_], [E3,_|_]).
checkDC([_,_|_], [_,_|_], [_,_|_], [_,_|_]).

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
countUnsolvedInMatrix(M,Y) :- flatten(M, Flat), countUnsolved(Flat,Y).

countUnsolved([],0).
countUnsolved([X|T], Y):- isUnknown(X), !,countUnsolved(T,Z), Y is 1+Z.
countUnsolved([X|T], Y):- countUnsolved(T,Y).

% Count the occurencies of the given value in a one-dimensional list. Excludes unknown (unbound) numbers
countUnsolvedDuplicates([],[],_,0).
countUnsolvedDuplicates([X|T],[A|B], V, Y):- isUnknown(A), X=V, !, countUnsolvedDuplicates(T,B,V,Z), Y is 1+Z.
countUnsolvedDuplicates([X|T],[A|B], V, Y):- countUnsolvedDuplicates(T,B,V,Y).