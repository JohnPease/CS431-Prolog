/* John Pease, CS431, homework 10 */
/* room(name, seat capacity, media capacity) */
room(r1, 30, media).
room(r2, 20, board).
room(r3, 25, media).

/* instructor (name, list of courses) */
instructor(i1, [c1, c2]).
instructor(i2, [c3, c4]).
instructor(i3, [c5, c6]). 

/*course(name, enrollment, media requirement, time (in hours)) */
course(c1, 25, media, 1).
course(c2, 30, media, 1.5).
course(c3, 20, board, 1.5).
course(c4, 25, media, 1).
course(c5, 30, media, 1.5).
course(c6, 20, board, 1.5).

/* schedule data for testing */

/* schedule(class, room, time period) */
schedule_data(
	      [schedule(c1, r3, between(9, 10)),
	       schedule(c2, r1, between(14, 15.5)),
	       schedule(c3, r2, between(14, 15.5)),
	       schedule(c4, r3, between(10, 11)),
	       schedule(c5, r1, between(11, 12.5)),
	       schedule(c6, r2, between(9, 10.5))]
	     ).

/* Define a predicate is_member(X, L) that returns true 
 * iff X is a member of list L */
is_member(X, L) :-
	member(X, L).

/* Define a predicate no_time_conflict(T1, T2) that returns true 
 * iff there is no time conflict between the time periods T1 and T2 
 * T1 is a between, T2 is a between */
no_time_conflict(T1, T2) :-
	between(A,B)=T1,
	between(C,D)=T2,
	(B=<C;D=<A).

/* Define a predicate has_instructor(C, I) that returns true 
 * iff C is a course taught by the instructor I */
has_instructor(C, I) :-
	instructor(I, [C,_]);
	instructor(I, [_,C]).

/* Define a predicate no_instructor_conflict(I1, T1, I2, T2) that returns true 
 * iff there is no conflict for instructor I1 to teach at time period T1
 * and for instructor I2 to teach at time period T2 */
no_instructor_conflict(I1, T1, I2, T2) :-
	not(I1=I2);
	no_time_conflict(T1,T2).

/* Define a predicate no_room_conflict(R1, T1, R2, T2) that returns true 
 * iff there is no conflict for room R1 to have class at time period T1
 * and for room R2 to have class at time period T2 */
no_room_conflict(R1, T1, R2, T2) :-
	not(R1=R2);
	no_time_conflict(T1,T2).

/* media_compatible(media type required by class, room media type) */
media_compatible(board, board).
media_compatible(media, media).
media_compatible(board, media).

/* Define a predicate legal_schedule(X) that returns true 
 * iff X is a legal schedule considering the capacity and media type of room, 
 * the enollment, media requirement, and duration of the class */
legal_schedule(X) :-
	schedule(C,R,between(Y,Z))=X,
	course(C, Numpeople, Mediareq, Time),
	room(R, People, Mediatype),
	(Z-Y)=:=Time,
	Numpeople=<People,
	media_compatible(Mediareq,Mediatype).

/* no_conflict(S1, S2) is true iff the schedules S1 and S2 are not in conflict */
no_conflict(schedule(C1, R1, T1), schedule(C2, R2, T2)) :-
	has_instructor(C1, I1), has_instructor(C2, I2),
	no_instructor_conflict(I1, T1, I2, T2),
	no_room_conflict(R1, T1, R2, T2).

/* Define a predicate check_conflict(S, L) that returns true iff 
 * S is not in conflict with any schedule in L */
check_conflict(S, []).
check_conflict(S, [Head|Tail]) :-
	no_conflict(S, Head),
	check_conflict(S, Tail).

/* legal_schedule_list(L) is true iff L is a legal list of schedules */
legal_schedule_list([]).
legal_schedule_list([S | L]) :- legal_schedule_list(L),
	legal_schedule(S),
	check_conflict(S, L).

test :- schedule_data(L), legal_schedule_list(L).


