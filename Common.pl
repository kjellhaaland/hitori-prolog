


rotateMatrix(M, Rotated) :-
    transpose(M, X),
    maplist(reverse, X, Rotated).