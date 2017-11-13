

phase1(Rows, Result, Size) :-
    equalityph1(Row, Result),
    patternSandwich(Rows, Result).



% Equality rule
equalityph1([],[]).
equalityph1([H1|T1], [H2|T2]) :- eqph1(H1,H2), equalityph1(T1,T2).

eqph1([],[]).
eqph1([H1|T1], [H2|T2]) :- (H1=H2  ; H2=0 ; H2=_), eqph1(T1,T2).



patternSandwich(M, EM) :- extractLineP1(M, EM), transpose(M,T), transpose(EM,TEM), extractLineP1(T,TEM).

extractLineP1([],[]).
extractLineP1([H|T],[A|B]) :- checkLineP1(H,A), extractLineP1(T,B).

checkLineP1([_,_],[_,_]).
checkLineP1([E1,E2,E1|T], [A1,E2,A3|B]) :- checkLineP1([E2,E3|T],[A2,A3|B]).
checkLineP1([E1,E1,E1|T], [0,E1,0|B]) :- checkLineP1([E2,E3|T],[A2,A3|B]).
checkLineP1([E1,E2,E3|T], [A1,A2,A3|B]) :- (E1\=E3; E1\=E2 ; E1\=E3), checkLineP1([E2,E3|T],[A2,A3|B]).
