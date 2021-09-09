% Η υλοποίηση έγινε με χρήση της βιβλιοθήκης ic της Eclipse

:- lib(ic).
:- [diags].

decode(Black_rows, Black_cols, Black_DiagsDown, Black_DiagsUp):-
	length(Black_rows, Rows),
	length(Black_cols, Cols),
	make_matrix(Rows, Cols, Matrix),
	matr_transp(Matrix, Transp),
	domain(Matrix),
	domain(Transp),
	constraints(Matrix, Black_rows),
	constraints(Transp, Black_cols),
	flatten(Matrix, Flatten_Matrix),
	diags(Matrix, DiagsDown, DiagsUp),
	domain(DiagsDown),
	domain(DiagsUp),
	constraints(DiagsDown, Black_DiagsDown),
	constraints(DiagsUp, Black_DiagsUp),
	search(Flatten_Matrix, 0, input_order, indomain, complete, []),
	pretty_print(Matrix).

domain([]).
domain([H | T]):-
	H #:: [0,1],
    domain(T). 

constraints([], []).
constraints([H | T], [L | R]):- 
	sum(H) #= L,
	constraints(T, R).

pretty_print([]).
pretty_print([H | T]):-
	pretty_print_line(H),
	pretty_print(T).

pretty_print_line([]):- nl.
pretty_print_line([H | T]):-
	H = 0,
	write(". "),
	pretty_print_line(T).
pretty_print_line([H | T]):-
	H = 1,
	write("* "),
	pretty_print_line(T).
	
make_matrix(M, N, Matrix) :-
   length(Matrix, M),
   make_lines(N, Matrix).

make_lines(_, []).
make_lines(N, [Line|Matrix]) :-
   length(Line, N),
   make_lines(N, Matrix).


