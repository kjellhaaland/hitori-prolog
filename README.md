# hitori-prolog
Hitori solver written in Prolog



## How to run the solver?
There are several ways to run the solver. If you want to solve a single hitori-board, you can do the following:

```prolog
% Open a terminal. (You need to have SWI-Prolog installed)

% Type *swipl* and press enter

% Load the solver by typing the following: 
['Solver'].

% Run the solver by typing the following: 
solveMatrix(5,5,[[1,2,5,4,3],[4,5,4,1,1],[1,1,3,1,4],[3,4,1,2,5],[3,1,4,1,2]], X).

```