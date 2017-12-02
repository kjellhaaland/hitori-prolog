/*
 * Phase 2 is a brute-force-ish algorithm for solving all kinds of matrices
 */
phase2(BM, SM, Size) :-
    fillMatrix(BM,SM,Size),
    rule1(SM),          % Check if there are any duplicate white items in the rows
    rule2(SM),          % Check if there are any adjacent black cells in the rows
    transpose(SM,TSM),  % Transpose the matrix to work with columns instad of rows
    rule1(TSM),         % Check if there are any duplicate white items in the columns
    rule2(TSM),         % Check if there are any adjacent black cells in the columns
    rule3(SM).          % Finally, check if all non-black cells is reachable. 

/*
 * Rule 1
 * Uses reccursion to search for possible duplicates in the same line
 * Transpose the matrix to check for columns: transpose(BM, SM).
 */
rule1(M) :- rule1Check(M).

rule1Check([]).
rule1Check([H|T]) :- checkLine2(H), rule1Check(T).

checkLine2(L) :- checkLine2(L,L).
checkLine2(_,[]).
checkLine2(L, [H|T]) :- isUnknown(H),!, checkLine2(L,T).
checkLine2(L, [H|T]) :- H = 0, checkLine2(L,T).
checkLine2(L, [H|T]) :- H \= 0, noDuplicates(L,H), checkLine2(L,T).

noDuplicates(L,V) :- noDuplicates(L,V,0). 
noDuplicates([],_,1).
noDuplicates([],_,0).
noDuplicates([V|T], X, R) :- isUnknown(V), noDuplicates(T,X,R).
noDuplicates([V|T], X, R) :- V=X, A is R+1, A<2, noDuplicates(T,X,A).
noDuplicates([V|T], X, R) :- V\=X , noDuplicates(T,X,R).


/*
 * Rule 2
 * Uses reccursion to search for possible adjacent black cells.
 * Transpose the matrix to check for columns: tranpose(BM,SM).
 */
rule2(M) :- rule2Check(M).

rule2Check([]).
rule2Check([H|T]) :- checkLine(H), rule2Check(T).

checkLine([_]) :- true.
checkLine([I1,I2|T]) :- not(blackPair(I1,I2)), checkLine([I2|T]). 

blackPair(I1,I2) :- I1 == 0, I2 == 0.


/*
 * Rule 3
 * Uses a sort of flood-filling algorithm to loop through the matrix and check if all non-black cells are connected.
 * 
 */
rule3(M) :-
    prepareForFlood(M,PM),      % Prepares the list for a effective flood-fill check
    firstNonBlack(PM,Start),    % Finds the first non-black cell in the matrix
    flood(PM,Start,[],R),       % Do the flooding algorithm
    flatten(M,FM),              % Flattens the original matrix. (Not the prepared matrix)
    nonBlacks(FM,NonBlacks),    % Filters away all of the black (0) cells.
    same_length(R, NonBlacks),!.    % If the cells visited by the floodfill has the same length as the list of all non-blacks, all cells are visited! 

/*
 * Floodfill algorithm
 * Needs a three dimensional matrix of cells. A cell is represented as a list of three elements.
 * Stores all visited cells in a set (list)
 */
flood(_,[V,_,_],Visited,Visited) :- V=0,!.                         % A black cell is visited, does not get added to visited cells
flood(_,[V,X,Y],Visited,Visited) :- member([V,X,Y], Visited),!.    % No new cells are visited, we are finished with this branch
flood(Matrix,[V,X,Y],Visited,R0) :-
    V \=0, append(Visited,[[V,X,Y]], R1),
    Xleft is X-1, elementAt(Matrix,Xleft,Y,Eleft),      % Finds the element to the right
    Xright is X+1, elementAt(Matrix,Xright,Y,Eright),   % Finds the element to the left
    Yup is Y-1, elementAt(Matrix,X,Yup,Eup),            % Finds the element above 
    Ydown is Y+1, elementAt(Matrix,X,Ydown,Edown),      % Finds the element underneath
    flood(Matrix,Eleft,R1,R2),      % Flood left
    flood(Matrix,Eright,R2,R3),     % Flood right
    flood(Matrix,Edown,R3,R4),      % Flood up
    flood(Matrix,Eup,R4,R5),!,      % Flood up
    list_to_set(R5,R0), !.


/*
 * Find the first element in the list that is non-black.
 * Because rule1 and rule2 is passed before, we know that one of the upper left cells must be white (non-black)
 */
firstNonBlack([H|_],V) :- exclude(ib,H, [V|_]).
ib([V,_,_]) :- V==0.

% Filter the given list. Return all non-black cells
nonBlacks(L, Result) :- exclude(isBlack, L, Result).
isBlack(X) :- X == 0.


% Utilizes nth0 to get the element at a given row and column
elementAt(M,X,Y,E) :- nth0(Y, M, Row), nth0(X,Row,E).
elementAt(_,_,_,[0,0,0]).

% Prepares a two dimensional matrix to the floodfilling algorithm. Stores the value, x-position and y-psoition in a list with three elements
prepareForFlood(A,B) :- prepareForFlood(A,B,0).
prepareForFlood([],[],_).
prepareForFlood([H|T], [A|B],Y) :- prepareRow(H,A,0,Y), Y2 is Y+1, prepareForFlood(T,B,Y2).

prepareRow([],[],_,_).
prepareRow([V|T1],[[V,X,Y]|T2],X,Y) :- X2 is X+1, prepareRow(T1,T2,X2,Y).


/*
 * Main solving component.
 * Flattens the matrices into lists. 
 * Paints the cells in the SolutionMatrix to either their original color or black
 * Performes chain reactions on each item painted. Skips already solved items
 */
fillMatrix(BM, SM, Size) :- 
    flatten(BM, FBM), flatten(SM, FSM), fillAlgorithm(FBM,FSM,Size).

fillAlgorithm(A,B,Size) :- fillAlgorithm(A,B,[],[],Size).
fillAlgorithm([],[],_,_,_).
fillAlgorithm([H1|T1], [H2|T2], A, B, Size) :- fill(A,B,[H1|T1],[H2|T2],Size), append(A,[H1],A2), append(B,[H2],B2), fillAlgorithm(T1,T2,A2,B2,Size).

% If the cell is already determined, skip
fill(_,_,[0|_],[_|_],_) :- !.

fill(H1,H2,[E1|T1],[E2|T2],Size) :- E2=E1,              % Set the cell to its original value
    append(H1,[E1|T1],A1), append(H2,[E2|T2],A2),       % Combine the head and tail of the list (to create the original list with the change)
    list2matrix(A1,Size,R1), list2matrix(A2,Size,R2),   % Transform the lists into matrices again
    doChainReactions(R1,R2).                              % Perform chain reactions

fill(H1,H2,[E1|T1],[E2|T2],Size) :- E2=0,               % Set the cell to black (painted)
    append(H1,[E1|T1],A1), append(H2,[E2|T2],A2),       % Combine the head and tail of the list (to create the original list with the change)
    list2matrix(A1,Size,R1), list2matrix(A2,Size,R2),   % Transform the lists into matrices again
    doChainReactions(R1,R2).                              % Perform chain reactions

% Length of the list
length_(Length, List) :- length(List, Length).

% Converts the given list into its original matrix-form, based on the size of the rows (Squares only)
list2matrix(List, RowSize, Matrix) :-
    length(List, L),
    HowManyRows is L div RowSize,
    length(Matrix, HowManyRows),
    maplist(length_(RowSize), Matrix),
    append(Matrix, List).


