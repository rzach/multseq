%            __  __ _   _ _ _                  
%           |  \/  | | | | | |_ ___  ___  __ _ 
%           | |\/| | | | | | __/ __|/ _ \/ _` |
%           | |  | | |_| | | |_\__ \  __/ (_| |
%           |_|  |_|\___/|_|\__|___/\___|\__, |
%                                           |_|
%
% A generic sequent prover for propositional many-valued logics,
% and much more ...
%
%                 Version 0.6 (13/09/2002)
%                 Angel Gil & Gernot Salzer

:- [msconf].    % OS- and Prolog-specific settings; currently only end-of-line
:- [msutil].    % auxiliary predicates
:- [mscalcul].  % proof construction, proof transformations, ...
:- [msconseq].  % consequence relations on sequents, formulas, equations
:- [mslgcin].   % input of logic specification
:- [msoption].  % option processing
:- [mstex].     % output routines (TeX)


test :-
	load_logic(luka),
	tell('out.tex'),
	cat('mspre.tex'),
	print_phrase(tex_title("Some Test Examples")),
	test([ts/2, tcs/3, tf/3, tcf/4, te/2, tqe/3]),
	cat('mspost.tex'),
	told,
	!.

test([]) :-
	!.
test([P|Ps]) :-
	!,
	test(P),
	test(Ps).
test(N/A) :-
	!,
	test_headline(N, HL),
	print_phrase(tex_section(HL)),
	functor(T, N, A),
	(  T,
	   test(T),
	   fail
	;  true
	).
test(T) :-
	test1(T),
	!.

test1(ts(N,S)) :-
	derivable(S, PT),
	collect_hyps(PT, Hs),
	proof_skeleton(PT, Sk, Sqs),
	counterexamples_l(Hs, Cexs),
	print_phrase(tex_report(seqcons, N, [], S, [S], Cexs,
           [hyps(Hs), deriv(PT), skel(Sk,Sqs)])).
test1(tcs(N,Hyps,Conc)) :-
	seqcons2seqs(Hyps, Conc, Ss),
	derivable_l(Ss, PTs),
	collect_hyps_l(PTs, Hs),
	counterexamples_l(Hs, Cexs),
	print_phrase(tex_report(seqcons, N, Hyps, Conc, Ss, Cexs, [])).
test1(tf(N,F,DTVs)) :-
	test1(tcf(N,[],F,DTVs)).
test1(tcf(N,Hyps,Conc,DTVs)) :-
	frmcons2seqs(Hyps, Conc, DTVs, S),
	derivable(S, PT),
	collect_hyps(PT, Hs),
	counterexamples_l(Hs, Cexs),
	print_phrase(tex_report(frmcons(DTVs), N, Hyps, Conc, [S], Cexs,
%           [hyps(Hs), deriv(PT)])).
           [])).
test1(te(N,E)) :-
	test1(tqe(N,[],E)).
test1(tqe(N,Hyps,Conc)) :-
	equcons2seqs(Hyps, Conc, Ss),
	derivable_l(Ss, PTs),
	collect_hyps_l(PTs, Hs),
	counterexamples_l(Hs, Cexs),
	print_phrase(tex_report(equcons, N, Hyps, Conc, Ss, Cexs, [])).

test_headline(ts, "Derivability of Sequents").
test_headline(tcs, "Consequence Relation on Sequents").
test_headline(tf, "Validity of Formulas").
test_headline(tcf, "Consequence Relation on Formulas").
test_headline(te, "Validity of Equations").
test_headline(tqe, "Validity of Quasi-Equations").
