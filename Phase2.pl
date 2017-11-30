
phase2(Rows, Result, Size) :-
    equalize(Rows,Result,Size),
    rule1(Result),
    rule2(Result),
    transpose(Result,Transposed),
    rule1(Transposed),
    rule2(Transposed),
    rule3(Result).

% Rule 1 - Checks if there are duplicates in a row or column
rule1(M) :- rule1Check(M).

rule1Check([]).
rule1Check([H|T]) :- checkLine2(H), rule1Check(T).

checkLine2(L) :- checkLine2(L,L).
checkLine2(_,[]).
checkLine2(L, [H|T]) :- H = 0, checkLine2(L,T).
checkLine2(L, [H|T]) :- H \= 0, noDuplicates(L,H), checkLine2(L,T).


noDuplicates(L,V) :- noDuplicates(L,V,0). 
noDuplicates([],_,1).
noDuplicates([],_,0).
noDuplicates([V|T], X, R) :- V=X, A is R+1, A<2, noDuplicates(T,X,A).
noDuplicates([V|T], X, R) :- V\=X , noDuplicates(T,X,R).


% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2(M) :- rule2Check(M).

rule2Check([]).
rule2Check([H|T]) :- checkLine(H), rule2Check(T).

checkLine([_]) :- true.
checkLine([I1,I2|T]) :- not(blackPair(I1,I2)), checkLine([I2|T]). 

blackPair(I1,I2) :- I1 == 0, I2 == 0.

% Rule 3
rule3(M) :-
    prepareForFlood(M,PM), 
    firstNonBlack(PM,Start),
    flood(PM,Start,[],R),
    flatten(M,FM), 
    nonBlacks(FM,NonBlacks),
    same_length(R, NonBlacks),!.


flood(Matrix,[V,X,Y],Visited,Visited) :- V=0,!.
flood(Matrix,[V,X,Y],Visited,Visited) :- member([V,X,Y], Visited),!.
flood(Matrix,[V,X,Y],Visited,R0) :-
    V \=0, append(Visited,[[V,X,Y]], R1),
    Xleft is X-1, elementAt(Matrix,Xleft,Y,Eleft),
    Xright is X+1, elementAt(Matrix,Xright,Y,Eright),
    Yup is Y-1, elementAt(Matrix,X,Yup,Eup),
    Ydown is Y+1, elementAt(Matrix,X,Ydown,Edown),
    flood(Matrix,Eleft,R1,R2),
    flood(Matrix,Eright,R2,R3),
    flood(Matrix,Edown,R3,R4),
    flood(Matrix,Eup,R4,R5),!, 
    list_to_set(R5,R0), !.

firstNonBlack([H|_],V) :- exclude(ib,H, [V|_]).
ib([V,X,Y]) :- V==0.

nonBlacks(L, Result) :- exclude(isBlack, L, Result).
isBlack(X) :- X == 0.


elementAt(M,X,Y,E) :- nth0(Y, M, Row), nth0(X,Row,E).
elementAt(M,X,Y,[0,0,0]).

prepareForFlood(A,B) :- prepareForFlood(A,B,0).
prepareForFlood([],[],V).
prepareForFlood([H|T], [A|B],Y) :- prepareRow(H,A,0,Y), Y2 is Y+1, prepareForFlood(T,B,Y2).

prepareRow([],[],_,_).
prepareRow([V|T1],[[V,X,Y]|T2],X,Y) :- X2 is X+1, prepareRow(T1,T2,X2,Y).


equalize(BM, SM, Size) :- 
    flatten(BM, FBM), flatten(SM, FSM), equality(FBM,FSM,Size).

equality(A,B,Size) :- equality(A,B,[],[],Size).
equality([],[],_,_,_).
equality([H1|T1], [H2|T2], A, B, Size) :- eq(A,B,[H1|T1],[H2|T2],Size), append(A,[H1],A2), append(B,[H2],B2), equality(T1,T2,A2,B2,Size).

eq(_,_,[0|_],[_|_],_) :- !.

eq(H1,H2,[E1|T1],[E2|T2],Size) :- E2=E1,
    append(H1,[E1|T1],A1), append(H2,[E2|T2],A2), 
    list2matrix(A1,Size,R1), list2matrix(A2,Size,R2),
    chainReactions(R1,R2).

eq(H1,H2,[E1|T1],[E2|T2],Size) :- E2=0,
    append(H1,[E1|T1],A1), append(H2,[E2|T2],A2), 
    list2matrix(A1,Size,R1), list2matrix(A2,Size,R2),
    chainReactions(R1,R2).

chainReactions(M,S) :-
    rotateMatrix(M, M1), rotateMatrix(S,S1), 
    patternStandardCycleBlack(M1,S1),!,
    rotateMatrix(M1, M2), rotateMatrix(S1,S2), 
    patternStandardCycleBlack(M2,S2),!, 
    rotateMatrix(M2, M3), rotateMatrix(S2,S3), 
    patternStandardCycleBlack(M3,S3),!, 
    rotateMatrix(M3, M4), rotateMatrix(S3,S4),
    patternStandardCycleBlack(M4,S4),
    patternStandardCycleWhite(M4,S4).
    

    
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


patternStandardCycleBlack([],[]).
patternStandardCycleBlack([E1|T],[A1|B]) :- checkStandardCycleBlack(E1,A1), patternStandardCycleBlack(T,B).

checkStandardCycleBlack([_],[_]).
checkStandardCycleBlack([E1,E2|T1], [A1,A2|B1]) :- ( isSolved(E1) ; isUnknown(A1) ), !, checkStandardCycleBlack([E2|T1], [A2|B1]).
checkStandardCycleBlack([E1,E2|T1], [A1,A2|B1]) :- A1=0, (A2=E2 ; E2=0), !, checkStandardCycleBlack([E2|T1], [A2|B1]).
checkStandardCycleBlack([E1,E2|T1], [A1,A2|B1]) :- A1\=0,!, checkStandardCycleBlack([E2|T1], [A2|B1]).



length_(Length, List) :- length(List, Length).
list2matrix(List, RowSize, Matrix) :-
    length(List, L),
    HowManyRows is L div RowSize,
    length(Matrix, HowManyRows),
    maplist(length_(RowSize), Matrix),
    append(Matrix, List).


isSolved(V) :- V=0.

isUnknown(V) :- integer(V), !, false.
isUnknown(V).