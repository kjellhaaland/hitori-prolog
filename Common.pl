
% Counts the occuriencies of the element X in the list.
count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).


rotateMatrix(M, Rotated) :-
    transpose(M, X),
    maplist(reverse, X, Rotated).