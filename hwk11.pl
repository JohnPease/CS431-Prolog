/* room(name, seat capacity, media capacity) */
room(r1, 30, media).
room(r2, 20, board).
room(r3, 25, media).

/* instructor (name, list of courses) */
instructor(i1, [c1, c2, c3]).
instructor(i2, [c4, c5, c6]).

/*course(name, enrollment, media requirement, time (in hours)) */
course(c1, 25, media, 1).
course(c2, 30, media, 1.5).
course(c3, 20, board, 1.5).
course(c4, 25, media, 1).
course(c5, 30, media, 1.5).
course(c6, 20, board, 1.5).

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

/* legal duration */
legal_duration(1).
legal_duration(1.5).

/* legal start time of a class */
start_time_list([11, 12, 14, 15]).

/* legal end time of a class */
end_time_list( [12, 12.5, 14, 14.5, 15, 15.5, 16, 16.5]).

/* 
 *  implement a legal_time(between(S, E)) predicate to generate legal combinations of S and E such that 
 *  1. S is from the list of the legal start time 
 *  2. E is from the list of the legal end time
 *  3. S-E is a legal duration
 */
legal_time(between(S, E)):-
	start_time_list(A),
	end_time_list(B),
	X is E-S,
	is_member(S, A), is_member(E, B), legal_duration(X).

/* 
 *  implement a legal schedule(schedule(C, R, between(S, E)) predicate that is true if
 *  1. between(S, E) is a legal time, 
 *  2. the capacity and media requirements of C are satisfied by R
 *  3. the duration of the class is no longer than E-S.
 */

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

gen_schedule(X) :- fixed_schedule(X), legal_schedule_list(X).

num_of_schedule(X) :- fixed_schedule(S),
	findall(1, legal_schedule_list(S), L),
	length(L, X).
