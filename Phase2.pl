
phase2(Rows, Result, Size) :-
    append(Result, L),
    L ins (0..Size),
    equality(Rows,Result),
    rule1(Result),
    rule2(Result),
    maplist(label, Result).    

% Rule 1 - Checks if there are duplicates in a row or column
rule1(M) :- rule1Check(M), transpose(M,T), rule1Check(T).

rule1Check([]).
rule1Check([H|T]) :- checkLine2(H), rule1Check(T).

checkLine2(L) :- checkLine2(L,L).
checkLine2(_,[]) :- !.
checkLine2(L, [H|T]) :- H = 0, checkLine2(L,T).
checkLine2(L, [H|T]) :- H \= 0, count(L,H,R), R=1, checkLine2(L,T).


% Rule 2 - Checks if there are adjacent blacks in a row or col
rule2(M) :- rule2Check(M), transpose(M,T), rule2Check(T).

rule2Check([]).
rule2Check([H|T]) :- checkLine(H), rule2Check(T).

checkLine([]) :- true.
checkLine([_]) :- true.
checkLine([I1,I2|T]) :- checkLine([I2|T]), not(blackPair(I1,I2)). 

blackPair(I1,I2) :- I1 == 0, I2 == 0.




% Alternative solution?
solve()


rule3(M,B) :- flatten(M, Flat), flatten(B, FlatBase), testBlacks(M,B).

testBlacks([E1,E2|T],[A1,A2|B]) :- E1=0, E2=A2, testBlacks([E2|T],[A2|B]).
testBlacks([E1,E2|T],[A1,A2|B]) :- E2=0, E1=A1, testBlacks([E2|T],[A2|B]).

% row(B,I,R) :- nth1(I, B, R).

% col(B,I,C) :- transpose(B, T), row(T,I,C).