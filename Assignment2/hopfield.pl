hopfield(M,W):-
    sum_yiT_x_yi(M,[],Sum),
    length(M, L),
    get_ith(1,M,V),
    length(V,N),
    m_I_n(L,N,I),
    matr_diff(Sum,I,W).

yiT_x_yi(Yi,Prod):-
    matr_transp(Yi,YiT),
    matr_mult(YiT, Yi, Prod).

sum_yiT_x_yi([],Acc,Acc).
sum_yiT_x_yi([],[],[]).
sum_yiT_x_yi([MH|MT],Acc,Sum):-
    yiT_x_yi([MH],X),
    matr_sum(X,Acc,Y),
    sum_yiT_x_yi(MT,Y,Sum).
    
add_lines([],[],[]).
add_lines([MH|MT], [NH|NT], [SH|ST]):-
	SH is MH + NH,
	add_lines(MT, NT, ST).

matr_sum([], [], []).
matr_sum(M, [], M).
matr_sum([MH|MT], [NH|NT], [SH|ST]):-
    add_lines(MH, NH, SH),
    matr_sum(MT, NT, ST).
    
make_matrix(M, N, Matrix) :-
   length(Matrix, M),
   make_lines(N, Matrix).

make_lines(_, []).
make_lines(N, [Line|Matrix]) :-
   length(Line, N),
   make_lines(N, Matrix).

make_square_matrix(N, Matrix) :-
   make_matrix(N, N, Matrix).

zeroes_line([]).
zeroes_line([H|T]):-
     H is 0,
     zeroes_line(T).
    
zeroes_matrix([]).
zeroes_matrix([H|T]):-
     zeroes_line(H),
     zeroes_matrix(T).
    
zeroes(N,Z):-
    make_square_matrix(N, Z),
    zeroes_matrix(Z).

%replaces an element of a list at position I with E
replace_element(I, N, [_|T], E, [E|T]):-
    I == N.
replace_element(I, N , [H|T], E, [K|L]):-
    I \= N,
    I1 is I + 1,
    K is H,
    replace_element(I1, N , T, E, L).
    
%replaces diagonal elements of a matrix with M
matr_replace_element(_,_,[],[]).
matr_replace_element(I,M,[H|T],[K|L]):-
    replace_element(1,I,H,M,K),
    I1 is I + 1,
    matr_replace_element(I1,M,T,L).

%computes the scalar product of number M with the identity matrix In of size N    
m_I_n(M,N,I):-
    zeroes(N,Z),
    matr_replace_element(1,M,Z,I).
    
matr_diff([], [], []).
matr_diff(M, [], M).
matr_diff([MH|MT], [NH|NT], [SH|ST]):-
    subtrack_lines(MH, NH, SH),
    matr_diff(MT, NT, ST).

subtrack_lines([],[],[]).
subtrack_lines([MH|MT], [NH|NT], [SH|ST]):-
  SH is MH - NH,
  subtrack_lines(MT, NT, ST).

get_ith(1, [X|_], X).
get_ith(I, [_|L], X) :-
   I > 1,
   I1 is I-1,
   get_ith(I1, L, X).

del_first([], [], []).
del_first([[X|L]|R], [X|RX], [L|RL]) :-
   del_first(R, RX, RL).

matr_transp(M, []) :-
   empty_lists(M).
matr_transp(M, [C|TM]) :-
   del_first(M, C, RM),
   matr_transp(RM, TM).

empty_lists([]).
empty_lists([[]|M]) :-
   empty_lists(M).

matr_mult(M1, M2, M3) :-
   matr_transp(M2, RM2),
   matr_transp_mult(M1, RM2, M3).

matr_transp_mult([], _, []).
matr_transp_mult([R1|M1], M2, [R3|M3]) :-
   matr_transp_mult1(R1, M2, R3),
   matr_transp_mult(M1, M2, M3).

matr_transp_mult1(_, [], []).
matr_transp_mult1(R1, [R2|M2], [X|R3]) :-
   inn_prod(R1, R2, X),
   matr_transp_mult1(R1, M2, R3).

inn_prod([], [], 0).
inn_prod([X1|R1], [X2|R2], IP1) :-
   inn_prod(R1, R2, IP2),
   IP1 is IP2 + X1 * X2.

