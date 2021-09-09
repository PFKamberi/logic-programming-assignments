diags(Matrix, DiagsDown, DiagsUp):-
    diags_down(Matrix, DiagsDown),
    diags_up(Matrix,DiagsUp).

diags_down([],[]).
diags_down(Matrix, DiagsDown):-
    mirror(Matrix,M),
    diags_up(M,X),
    reverse(X,DiagsDown).
    
diags_up([],[]).
diags_up(Matrix, DiagsUp):-
    length(Matrix, L),
    pad(Matrix, L, L, [], Padded),
    matr_transp(Padded,Transposed),
    length(Transposed,LT),
    clean(Transposed,LT,[],DiagsUp).

pad(Matrix, L,1, Acc, Padded):-
    get_ith(1, Matrix, Ith),
    I2 is L - 1,
    pad_back(Ith,I2,AC1),
    append([AC1],Acc, Padded).
pad(Matrix, L,I , Acc,Padded):-
    I > 1,
    get_ith(I, Matrix, Ith),
    I1 is I - 1,
    pad_front(Ith, I1, AC),
    I2 is L - I,
    pad_back(AC,I2,AC1),
    append([AC1],Acc, Acc1),
    pad(Matrix, L, I1, Acc1, Padded).

pad_front(Line,0,Line).
pad_front(Line, 1, Padded_Line):-
    append([*],Line, Padded_Line).
pad_front(Line, Padding ,Padded_Line):-
	Padding > 1,
    append([*],Line, X),
    Padding1 is Padding - 1,
    pad_front(X, Padding1 ,Padded_Line).

pad_back(Line,0,Line).
pad_back(Line, 1, Padded_Line):-
    append(Line, [*] ,Padded_Line).
pad_back(Line, Padding ,Padded_Line):-
	Padding > 1,
    append(Line, [*] ,X),
    Padding1 is Padding - 1,
    pad_back(X, Padding1 ,Padded_Line).

remove_padding(_, [], []).
remove_padding(X, [X|T], L):- remove_padding(X, T, L), !.
remove_padding(X, [H|T], [H|L]):- remove_padding(X, T, L ).

clean(M,1,Acc,Cleaned):-
    get_ith(1,M,Ith),
    remove_padding(*,Ith, X),
    append([X], Acc, Cleaned).
clean(M,I,Acc,Cleaned):-
    I > 1,
    get_ith(I,M,Ith),
    remove_padding(*,Ith, X),
    append([X], Acc, Acc1),
    I1 is I - 1,
    clean(M,I1,Acc1,Cleaned).
 
mirror([],[]).
mirror([H|Ts],[H1|R]):-
    reverse(H,H1),
    mirror(Ts,R).
    
get_ith(1, [X|_], X).
get_ith(I, [_|L], X) :-
   I > 1,
   I1 is I-1,
   get_ith(I1, L, X).

del_first([], [], []).
del_first([[X|L]|R], [X|RX], [L|RL]) :-
   del_first(R, RX, RL).

empty_lists([]).
empty_lists([[]|M]) :-
   empty_lists(M).

matr_transp(M, []) :-
   empty_lists(M).
matr_transp(M, [C|TM]) :-
   del_first(M, C, RM),
   matr_transp(RM, TM).

