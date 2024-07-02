:- dynamic
	truth_values_orig/1,
	truth_values/1,
	designated_truth_values_orig/1,
	designated_truth_values/1,
	rule/3,
	ts/2, tcs/3, tf/3, tcf/4, te/2, tqe/3,
	tex_rn/2, 
	tex_tv/2, 
	tex_op/2, 
	tex_opname/2,
	operator/2,
	operator_tex/2.

lgcDatabase([
	truth_values_orig/1,
	truth_values/1,
	designated_truth_values_orig/1,
	designated_truth_values/1,
	rule/3,
	ts/2, tcs/3, tf/3, tcf/4, te/2, tqe/3,
	tex_rn/2, 
	tex_tv/2, 
	tex_op/2, 
	tex_opname/2,
	operator/2,
	operator_tex/2
]).

load_logic(F) :-
	clear(lgcDatabase),
	read_logic(F),
	!.

read_logic(F) :-
	see(F),
	repeat,
	read(T),
	process(T),
	seen,
	!.

process(end_of_file).
process(T) :-
	process1(T),
	!, fail.

process1(truth_values(TVs0)) :-
	sort(TVs0, TVs1),
	assertz(truth_values_orig(TVs0)),
	assertz(truth_values(TVs1)).
process1(designated_truth_values(TVs0)) :-
	sort(TVs0, TVs1),
	assertz(designated_truth_values_orig(TVs0)),
	assertz(designated_truth_values(TVs1)).
process1(rule(F, Ps0, R)) :-
	sort_l(Ps0, Ps1),
	assertz(rule(F, Ps1, R)).
process1(op(P,T,N)) :-
	op(P,T,N).
process1(operator(O, A, T)) :-
	assertz(operator(O, A)),
	assertz(tex_opname(O, [T])),
	assertz(operator_tex(O) --> T).
process1(ts(N,S)) :-
	assertz(ts(N,S)).
process1(tcs(N,Hs,C)) :-
	assertz(tcs(N,Hs,C)).
process1(tf(N,F,DTVs)) :-
	assertz(tf(N,F,DTVs)).
process1(tcf(N,Hs,C,DTVs)) :-
	assertz(tcf(N,Hs,C,DTVs)).
process1(te(N,E)) :-
	assertz(te(N,E)).
process1(tqe(N,Hs,C)) :-
	assertz(tqe(N,Hs,C)).
process1(option(O)) :-
	set_option(O).
process1(tex_rn(R, RT)) :-
	assertz(tex_rn(R, RT)).
process1(tex_tv(S, ST)) :-
	assertz(tex_tv(S, ST)).
process1(tex_op(O, OT)) :-
	assertz(tex_op(O, OT)).
process1(tex_opname(O, OT)) :-
	assertz(tex_opname(O, OT)).
process1(X) :-
	print_message(warning, lgcin_ignored(X)).

prolog:message(lgcin_ignored(X)) -->
	[ 'Ignoring ~w'-[X] ].