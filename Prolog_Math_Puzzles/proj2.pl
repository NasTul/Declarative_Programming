%  Author   : Xiangyu Gao
%  Origin   : Wed Oct 11 15:50:00 2017
%  Purpose  : This project is to solve the maths puzzle by implementing the logic 
%  programming and Prolog. Given the constraints of the puzzle, the program can do 
%  the math behind and fill the unground squares. Each proper maths puzzle will have
%  at most one solution.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The goal of the maths puzzle is to be fill in each square with a single digit 1-9.
%% A maths puzzle will be represented as a list of lists, each of the same length, 
%% representing a single row of the puzzle. Ahead of each row and column is the number
%% that holds either the sum or the product of all the digits in that row or column. 
%% All the values on the diagonal from upper left to lower right is same.


%% Load the library module to reason declarative integer arithmetic and solving 
%% combinatorial problems.
:- ensure_loaded(library(clpfd)).


%% puzzle_solution(Puzzle)
%% Hold when Puzzle is the representation of a solved maths puzzle. First, unifying all 
%% the squares on the diagonal. Second, transpose the Puzzle and make the columns bacome rows.
%% Third, remove the heading of each row and column to construct a new unsolved puzzle. Forth, 
%% solve each list to satisfy each element but the first of the first list in the puzzle is 
%% either the sum or the product of all the digits. Then label each row.

puzzle_solution(Puzzle) :-
    equal_diagonal(Puzzle),
    transpose(Puzzle, Cols),
    delete_head(Puzzle, R1),
    delete_head(Cols, R2),
    append(R1, R2, Rows),
    test_valid(Rows),
    maplist(label, Rows).


%% test_valid(Rows)
%% Solve the modified puzzle row by row. If the first element of this row is equal to the sum 
%% or the product of all the digits in that row, go to the next row and do it recursively.

test_valid([]).
test_valid([R1|Rs]) :-
    (sum(R1); product(R1)),
    test_valid(Rs).


%% sum(Row)
%% Solve one unground row to satisfy the sum condition and constraint sum solver to meet the 
%% arithmetic condition. Each to be filled in with a single digit 1-9 (zero is not permitted).

sum([H|Ts]) :-
    number(H),
    Vars = Ts,
    Vars ins 1..9,
    all_distinct(Vars),
    sum(Vars, #=, H).


%% product(Row)
%% Solve one unground row to satisfy the product condition and constraint product solver to meet
%% the arithmetic condition. Each to be filled in with a single digit 1-9 (zero is not permitted).

product([H|Ts]) :-
    number(H),
    Vars = Ts,
    Vars ins 1..9,
    all_distinct(Vars),
    product_list(Vars, H).


%% product_list(List, Pro)
%% Do the product computation for each element of the lists recursively so that each row can be 
%% constrained to the fact that Pro is the product of elements.

product_list(List, Pro) :-
    product_list(List, 1, Pro).

product_list([], Pro, Pro).
product_list([X|Xs], Temp, Pro) :-
    Temp1 #= Temp * X,
    product_list(Xs, Temp1, Pro).


%% equal_diagonal(Puzzle)
%% The element on the diagonal line is a list of finite domain variables that are a 
%% chain with respect to the relation of equality.

equal_diagonal(Puzzle) :-
    diagonal(Puzzle, Dias),
    delete_head(Dias, Dia1),
    chain(Dia1, #=).


%% diagonal(Puzzle, Dias)
%% Extract the element on the diagonal by choose the first element of each line and remove the 
%% next line's header. Do these two steps recursively to get the diagonal elements of that Puzzle.

diagonal([], []).
diagonal([[H|_]|Rows], [H|T]) :-
    remove_heads(Rows, X), 
    diagonal(X, T).


%% remove(Row, X)
%% Each time pop out the first element of the list so the first lement of next line can be a 
%% diagonal candidate next round.

remove_heads([],[]).
remove_heads([[_|T]|Rows], [T|X]) :-
    remove_heads(Rows, X).


%% delete_head(Puzzle, Row)
%% Delete the first elemment of the list of lists. The lists left is the rows/columns to be solved. 

delete_head([_|T], T).
