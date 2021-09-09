:- lib(ic).
:- lib(branch_and_bound).  

games_csp(Pi, T, K, Gs, P):-
	length(Pi, L),
	length(Gs, L),
	domain(Gs, T),
	neg_Pi_constraint(Pi, Gs),
	constraints(Gs, T, T, K),
	inn_prod(Pi, Gs, Pl),
	P1 #= -Pl, 
	bb_min(search(Gs, 0, max_regret  , indomain_reverse_split , complete , []), P1, bb_options{strategy:continue, delta:2, solutions:all}),
	P is -P1.
%dbs(L, lds(T))

neg_Pi_constraint([],[]).
neg_Pi_constraint([L | R], [M | N]):-
	L < 0,
	M #= 1,
	neg_Pi_constraint(R, N).
neg_Pi_constraint([L | R], [M | N]):-
	L >= 0,
	neg_Pi_constraint(R, N).

domain([],_).
domain([L | R], T):-
	L #:: [1..T],
    domain(R, T). 

inn_prod([], [], 0).
inn_prod([X1|R1], [X2|R2], IP1) :-
   IP1 #= IP2 + X1 * X2,
   inn_prod(R1, R2, IP2).

constraints([], _, _, _).
constraints([L | R], Curr, T, K):- 
	L #=< Curr,
	Curr1 #= min(Curr - L + K,T),
	constraints(R, Curr1, T, K).
