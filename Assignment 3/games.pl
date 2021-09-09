games(Pl, T, K, Gs, P):-
    generate_states(Pl, T, K, States),
    get_ith(1, States, Ith),
    evaluate_state(Ith,Pl,0,IthEval),
    find_best(States, Pl, IthEval, [], BS),
    evaluate_state(BS,Pl,0,P), 
    member(X, States),evaluate_state(X,Pl,0,E), E = P,
    strip_slashes(X,Gs).

get_ith(1, [X|_], X).
get_ith(I, [_|L], X) :-
   I > 1,
   I1 is I-1,
   get_ith(I1, L, X).

make_tmpl(G, T, T, [G/T]).
make_tmpl(G, I, T, [G/I|Rest]) :-
   I < T,
   I1 is I+1,
   make_tmpl(G, I1, T, Rest).

plus_K(Marks, T, K, NewMarks):-
    Marks + K > T,
    NewMarks is T.
plus_K(Marks, T, K, NewMarks):-
    Marks + K =< T,
    NewMarks is Marks + K.

generate_states(Pl, T, K, States):-
    length(Pl, N),
    possible_playings(Pl,1,N,T,X),
    cartesian_product(X, AllStates),
    findall(State, (member(State,AllStates), legal_state(State,T,T,K)), States).

% How many times a game can be played. Returns a list of list of 
% elements with form G/I, where G is the ascending number of the game and I is the times 
% it could be played.
% E.g. ?- possible_playings([4,1,2,3], 1, 4, 5, X).
%  X = [[1/1, 1/2, 1/3, 1/4, 1/5], [2/1, 2/2, 2/3, 2/4, 2/5], [3/1, 3/2, 3/3, 3/4, 3/5], [4/1, 4/2, 4/3, 4/4, 4/5]]
possible_playings([PT], N, N, T,[R]):-
    PT >= 0,
  make_tmpl(N, 1, T ,R).
possible_playings([PT], N, N, _, [R]):-
    PT < 0,
  make_tmpl(N, 1, 1,R).
possible_playings([PH | PT], I, N, T, [L | R]):-
  I < N,
  PH >= 0,
  make_tmpl(I, 1, T ,L),
  I1 is I+1,
  possible_playings(PT, I1, N, T,  R).
possible_playings([PH | PT], I, N, T, [L | R]):-
  I < N,
  PH < 0,
  make_tmpl(I, 1, 1 ,L),
  I1 is I+1,
  possible_playings(PT, I1, N, T,  R).

cartesian_product(S, L) :-
   findall(R, cart(S, R), L).
cart([], []).
cart([[A | _] | T], [A | R]) :-
   cart(T, R).
cart([[_ | B] | T], R) :-
   cart([B | T], R).
    
legal_state([],_,_,_).
legal_state([_/I| R], Curr,T, K):-
   Curr1 is Curr - I,
   Curr1 >= 0,
   plus_K(Curr1, T, K,  Curr2),
   legal_state(R, Curr2, T, K).
   
% Computes the total pleasure of a state
evaluate_state([],[], Acc, Acc).
evaluate_state([_/I| R], [H | T], Acc, E):-
    Acc1 is Acc + I*H,
    evaluate_state(R, T, Acc1, E).

% Finds a state with maximum total Pleasure
find_best([],_,_,Acc,BestState):-
    BestState = Acc.
find_best([H | T], Pl, Best, _, BestState):-
    evaluate_state(H, Pl, 0, E),
    E >= Best,
    find_best(T, Pl, E, H, BestState).
find_best([H | T], Pl, Best, Acc, BestState):-
    evaluate_state(H, Pl, 0, E),
    E < Best,
    find_best(T, Pl, Best, Acc,  BestState).

strip_slashes([], []).
strip_slashes([_/I| R], [H | T]):-
    H is I,
    strip_slashes(R, T).


    