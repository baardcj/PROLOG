% DPLL algorithm - Davis–Putnam–Logemann–Loveland
% example input : inp(X, [[1, 2, 3], [2, 3, 4], ...]).


inp(Y, A):-
        cut(X, A),              % - atomic cut method
        flatten(A, B),          % - flattens the input of clauses into a regular list
        makeModel(X, Y, B).     % - get arbitrarymodel when needed those litterals that did not need to be assigned a particular truth-value to satisfy the input-formula
                                %   are given an arbitrary truth value to complete model 


cut([], []) :-  !, true.

cut([Lit|T], A) :-
          getNextLiterals(Lit, NegLit, A),    % - helper method to get the negation of a liteal
          unit_sub(X, Lit, A),                % - unit subsumption
          unit_res(Y, NegLit, X),             % - unit resolution  
          cut(T, Y).                          % - recursion

cut([NegLit|T], A) :-                         % same as above but literal is negated  
          getNextLiterals(Lit, NegLit, A),
          unit_sub(X, NegLit, A),
          unit_res(Y, Lit, X),
          cut(T, Y).

getNextLiterals(Lit, NegLit, [[Lit|_]|_]):- NegLit is -1 * Lit.


unit_sub([X1|[]], Lit, [X1|[]]) :- \+ member(Lit, X1), !.                           % delete clause if the literal in question is member of clause
unit_sub([X1|T1], Lit, [X1|T2]) :- \+ member(Lit, X1), !, unit_sub(T1, Lit, T2).    % same as above
unit_sub([], _, [_|[]]).                                                            % base case - the literal in question is not member of clause, and the clause is cept.
unit_sub(A, Lit, [_|T2]) :-  unit_sub(A, Lit, T2).                                  % same as above (but not base case)

unit_res([], _, []) :- !, true.                                                     % 
unit_res([X1|[]], Lit, [X1|[]]) :- \+ member(Lit, X1), !.                           % base case - literal is not member of next clause
unit_res([X1|T1], Lit, [X1|T2]) :- \+ member(Lit, X1), !, unit_res(T1, Lit, T2).    % literal is not member of next clause
unit_res([_|_], Lit, [X1|_]) :- [Lit] == X1, !, fail.                               % literal is member and deleting it made the clause empty - hence FAIL!
unit_res([A|[]], Lit, [X1|[]]) :- !, delete(X1, Lit, A).                            % base case - literal is member, remove it from clause
unit_res([A|T1], Lit, [X1|T2]) :- delete(X1, Lit, A), unit_res(T1, Lit, T2).        % literal is member , remove it from clause



% makeModel() - needed to give remaining values to literals of model. 

makeModel(_, [], []) :- !.

% below: step-2 - append first element from list of literals not interpreted to new model ([H1|T2]), and remove that literal and the negation of that literal from the list of literals not interpreted.

makeModel([], [H1|T2], [H1|Lits]):-
          delete(Lits, H1, RemLits_1),
          NegLit is -1 * H1,
          delete(RemLits_1, NegLit, RemLits_2),
          makeModel([], T2, RemLits_2).

% below: step 1 - append first element from derived model to a new list, and remove that element from those literals (Lits) that need to be given a truth value.

makeModel([H1|T1], [H1|T2], Lits):-
          delete(Lits, H1, RemLits_1),
          NegLit is -1 * H1,
          delete(RemLits_1, NegLit, RemLits_2),
          makeModel(T1, T2, RemLits_2).


delete([],_,[]).
delete([X1|T],X,L) :- X==X1, delete(T,X,L).
delete([X1|T],X,[X1|L]) :- X\==X1, delete(T,X,L).
