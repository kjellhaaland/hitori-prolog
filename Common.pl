


rotateMatrix(M, Rotated) :-
    transpose(M, X),
    maplist(reverse, X, Rotated).

% Gets the row of the board(B) given by the index(I)
row(B,I,R) :- nth0(I, B, R).

% Gets the col of the board(B) given by the index(I)
col(B,I,C) :- transpose(B, T), row(T,I,C).