
% Gets the row of the board(B) given by the index(I)
row(B,I,R) :- nth1(I, B, R).

% Gets the col of the board(B) given by the index(I)
col(B,I,C) :- transpose(B, T), row(T,I,C).


% Equality rule
equality([],[]).
equality([H1|T1], [H2|T2]) :- eq(H1,H2), equality(T1,T2).

eq([],[]).
eq([H1|T1], [H2|T2]) :- (H1=H2  ; H2=0 ), eq(T1,T2).


% Counts the occuriencies of the element X in the list.
count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).


rotateMatrix(M, Rotated) :-
    transpose(M, X),
    maplist(reverse, X, Rotated).