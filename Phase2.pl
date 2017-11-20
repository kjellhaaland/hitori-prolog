
phase2(Rows, Result, Size) :-
    equality(Rows,Result),
    rule1(Result),
    rule2(Result),
    transpose(Result,Transposed),
    rule1(Transposed),
    rule2(Transposed).

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

checkLine([]) :- true.
checkLine([_]) :- true.
checkLine([I1,I2|T]) :- checkLine([I2|T]), not(blackPair(I1,I2)). 

blackPair(I1,I2) :- I1 == 0, I2 == 0.


% Equality rule
equality([],[]).
equality([H1|T1], [H2|T2]) :- eq(H1,H2), equality(T1,T2).

eq([],[]).
eq([H1|T1], [H2|T2]) :- (H1=H2  ; H2=0 ), eq(T1,T2).