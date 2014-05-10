/* Define a predicate sum(L,S) that returns true if S is the sum of all the numbers in L */
sum([], 0).
sum([H|T], S) :-
	X is S-H,
	sum(T, X).

/* Define a predicate subset(L, SubL) that takes a list L of numbers and unifies SubL with subsequence of L */
subset(L, SubL):-
	append(L, SubL, L),
	writeln(X).


