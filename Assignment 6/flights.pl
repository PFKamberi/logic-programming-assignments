:- [flight_data].
:- lib(ic).
:- lib(branch_and_bound). 

flights(I,Pairings,Cost) :-
	get_flight_data(I, N, P, C),
	length(P, M),
	length(X, M),
	flight_list(N, FL),
	domain(X),
	constraints(FL, X, P),
	cost(X, C, 0, Cost),
	Cost  #> 0, 
	bb_min(search(X, 0, max_regret, indomain_reverse_split , complete , []), Cost, bb_options{strategy:continue, delta:2, solutions:all}),
	get_pairings(X,P,C, [], Pairings).

flight_list(0, []).
flight_list(N, [H | T]):-
	N > 0,
	N1 is N - 1,
	H is N,
	flight_list(N1, T).

domain([]).
domain([H | T]):-
	H #:: [0,1],
    domain(T). 

constraints([], _, _).
constraints([F | T], X, P):-
	constraints(F, X, P, [], _),
	constraints(T, X, P).

constraints(_, [], [], L, L):-
	sum(L) #= 1.
constraints(F,  [X | T], [P | R], Acc, L):-
	member(F, P),
	append([X], Acc, Acc1),
	constraints(F, T, R, Acc1, L).
constraints(F,  [_ | T], [P | R], Acc, L):-
	not member(F, P),
	constraints(F, T, R, Acc, L).

cost([], [], C, C).
cost([XH | XT], [CH | CT],  Acc, C):-
	Acc1 #= XH*CH + Acc,
	cost(XT, CT, Acc1, C).

get_pairings([],[],[], Acc,L):-
	reverse(Acc, L).
get_pairings([XH | XT], [PH | PT], [CH | CT], Acc, L):-
	XH = 1,
	append([PH / CH], Acc, Acc1),
	get_pairings(XT, PT, CT, Acc1, L).
get_pairings([XH | XT], [_ | PT], [_ | CT], Acc, L):-
	XH = 0,
	get_pairings(XT, PT, CT, Acc, L).