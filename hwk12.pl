/* John Pease, CS431, Homework 12 */

/* room(name, seat capacity, media capacity) */
room(r1, 25, media).
room(r2, 30, board).
room(r3, 30, media).

/* instructor (name, list of courses) */
instructor(i1, [c1, c2, c3]).
instructor(i2, [c4, c5, c6]).
/* instructor(i3, [c5, c6]). */

/*course(name, enrollment, media requirement, time (in hours)) */
course(c1, 25, media, 1).
course(c2, 30, media, 1.5).
course(c3, 20, board, 1.5).
course(c4, 25, media, 1).
course(c5, 30, media, 1.5).
course(c6, 20, board, 1.5).


/* schedule(class, room, time period) */

is_member(X, [X|_]).
is_member(X, [_|L]) :- is_member(X, L).

no_time_conflict(between(_, E1), between(S2, _)) :- E1 =< S2.
no_time_conflict(between(S1, _), between(_, E2)) :- E2 =< S1.

has_instructor(C, I) :- instructor(I, L), is_member(C, L).
no_instructor_conflict(I, T1, I, T2) :- no_time_conflict(T1, T2).
no_instructor_conflict(I1, _, I2, _) :- not(I1 = I2).

no_room_conflict(R, T1, R, T2) :- no_time_conflict(T1, T2).
no_room_conflict(R1, _, R2, _) :- not(R1 = R2).

media_compatible(board, board).
media_compatible(media, media).
media_compatible(board, media).

legal_duration(1).
legal_duration(1.5).
legal_time(between(S, E)) :-
	is_member(S, [11, 12, 14, 15]),
	is_member(E, [12, 12.5, 14, 14.5, 15, 15.5, 16, 16.5]),
	legal_duration(D),
	D =:= E - S.

legal_schedule(schedule(C, R, between(S, E)))  :-
        legal_time(between(S, E)),
	course(C, C1, M1, D),  room(R, C2, M2),
	C1 =< C2,
	S + D =< E,
	media_compatible(M1, M2).


no_conflict(schedule(C1, R1, T1), schedule(C2, R2, T2)) :-
	has_instructor(C1, I1), has_instructor(C2, I2),
	no_instructor_conflict(I1, T1, I2, T2),
	no_room_conflict(R1, T1, R2, T2).

check_conflict(_, []).
check_conflict(S, [S1 | L]) :- no_conflict(S, S1), check_conflict(S, L).

legal_schedule_list([]).
legal_schedule_list([S | L]) :- legal_schedule_list(L),
	legal_schedule(S),
	check_conflict(S, L).

fixed_schedule([schedule(c1,_,_), schedule(c2,_,_), schedule(c3,_,_),
	      schedule(c4,_,_), schedule(c5,_,_), schedule(c6,_,_)]).


gen_schedule_list(L) :- fixed_schedule(S),
	findall(S, legal_schedule_list(S), L).

/* implement excess_capacity_one(schedule(C, R, _), X) */
excess_capacity_one(schedule(C, R, _), X) :-
	course(C, CNumEnrolled, _, _),
	room(R, RCapacity, _),
	X is RCapacity-CNumEnrolled.

/* implement excess_capacity(L, X) */
excess_capacity([], X) :- write('C = '),writeln(X).
excess_capacity([H|T], X) :-
	(integer(X) -> X is X; X is 0),
	excess_capacity_one(H, R),
	A is X+R,
	excess_capacity(T, A).

/* implement minc(L, SofarC, Sofar, X) */
minc(L, SofarC, Sofar, X).

/* implement min_excess_capacity(L, X) */
min_excess_capacity(L, X).

/* implement best_schedule(X, C) */
best_schedule(X, C).

num_best_schedule(N) :-
	findall(1, best_schedule(_,_), L),
	length(L, N).
