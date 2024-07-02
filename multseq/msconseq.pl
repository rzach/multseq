% ss_sas(SS, SAS).
%   <SS> and <SAS> both denote the same sequent, with
%   the only difference that in <SS> the signs are single
%   truth values (SS = singleton signs), whereas in <SAS>  
%   they are sets (SAS = sets as signs).
%   The predicate works both directions.

ss_sas(SS, SAS) :-
	(nonvar(SS) ->
	   ss_sas1(SS, SAS)
	;  nonvar(SAS),
	   ss_sas2(SAS, SS)
	).

ss_sas1([], []).
ss_sas1([A^S|Ls0], [A^[S|Ss]|Ls]) :-
	ss_sas1(Ls0, A, Ss, Ls).

ss_sas1([], _A, [], []).
ss_sas1([A^S|Ls0], A, [S|Ss], Ls) :-
	!,
	ss_sas1(Ls0, A, Ss, Ls).
ss_sas1([B^S|Ls0], _A, [], [B^[S|Ss]|Ls]) :-
	ss_sas1(Ls0, B, Ss, Ls).

ss_sas2([], []). 
ss_sas2([A^Ss|Ls0], Ls) :-
	ss_sas2(Ss, A, Ls0, Ls).

ss_sas2([], _A, Ls0, Ls) :-
	ss_sas2(Ls0, Ls).
ss_sas2([S|Ss], A, Ls0, [A^S|Ls]) :-
	ss_sas2(Ss, A, Ls0, Ls).

:- ss_sas([a^1,a^2,a^3,b^1,b^2,c^1],SAS),
   SAS = [a^[1, 2, 3], b^[1, 2], c^[1]],
   ss_sas(SS, [a^[1, 2, 3], b^[1, 2], c^[1]]),
   SS = [a^1, a^2, a^3, b^1, b^2, c^1].

% ss_sas_l(Formulas, Signs, Signed Formulas)
%   <Signed Formulas> is obtained by signing every
%   formula in <Formulas> with every sign in <Signs>.

ss_sas_l(Fs, Ss, FSs) :-
	ss_sas_l(Fs, Ss, FSs, []).

ss_sas_l([], _Ss, FSs, FSs).
ss_sas_l([F|Fs], Ss, FSs, FSs0) :-
	ss_sas_l1(Ss, F, FSs, FSs1),
	ss_sas_l(Fs, Ss, FSs1, FSs0).

ss_sas_l1([], _F, FSs, FSs).
ss_sas_l1([S|Ss], F, [F^S|FSs], FSs0) :-
	ss_sas_l1(Ss, F, FSs, FSs0).

:- ss_sas_l([a,b], [x,y],  [a^x, a^y, b^x, b^y]).

% seqcons2seqs(Hyps, Concl, Seqs)
%   Transforms the consequence relation on sequents
%   to a set of sequents, i.e.,
%   the conclusion <Concl> (a single sequent) follows from
%   the hypotheses <Hyps> (a list of sequents) iff all
%   sequents in <Seqs> are valid.

seqcons2seqs(Hyps, Concl, Seqs) :-
	findall(Seq, seqcons2seq(Hyps, Concl, Seq), Seqs0),
	remove_axioms(Seqs0, Seqs1),
	minimize_seqs(Seqs1, Seqs).

seqcons2seq(Hyps, Concl, Seq) :-
	truth_values(TVs),
	negate_seqs(Hyps, TVs, NSeq0),
	ss_sas(NSeq1, NSeq0),
	ms_flatten([Concl, NSeq1], Seq0),
	sort(Seq0, Seq).
	
negate_seqs([], _TVs, []).
negate_seqs([Seq0|Seqs], TVs, [A^NSs|NSeq]) :-
	ss_sas(Seq0, Seq1),
	ms_member(A^Ss, Seq1),
	diff(TVs, Ss, NSs),
	negate_seqs(Seqs, TVs, NSeq).

% frmcons2seq(Hyps => Concl, Seq)
%   Transforms the consequence relation on formulas
%   to a sequent, i.e.,
%   the conclusion <Concl> (a single formula) follows from
%   the hypotheses <Hyps> (a list of formulas) iff the
%   sequent <Seq> is valid.
% (Right now uses designated truth value preservation for 
% consequence and assuming whatver designated_truth_values(X)
% says are the designated values. May test other consequence
% relations later (strict-tolerant, order preserving...).

frmcons2seq(Hyps => Concl, Seq) :-
	truth_values(TVs),
	designated_truth_values(DTVs0),
	sort(DTVs0, DTVs),
	diff(TVs, DTVs, CDTVs),
	ss_sas_l(Hyps, CDTVs, SeqH),
	ss_sas_l([Concl], DTVs, SeqC),
	ms_append(SeqC, SeqH, Seq0),
	sort(Seq0, Seq).

frmcons2seqs(Hyps => Concl, DTVs0, Seq) :-
	truth_values(TVs),
	sort(DTVs0, DTVs),
	diff(TVs, DTVs, CDTVs),
	ss_sas_l(Hyps, CDTVs, SeqH),
	ss_sas_l([Concl], DTVs, SeqC),
	ms_append(SeqC, SeqH, Seq0),
	sort(Seq0, Seq).

% equcons2seqs(Hyps, Concl, Seqs)
%   Transforms the consequence relation on equation
%   (also called quasi-equations) to a set of sequents, i.e.,
%   the conclusion <Concl> (a single equation) follows from
%   the hypotheses <Hyps> (a list of equations) iff all
%   sequents in <Seqs> are valid.

equcons2seqs(Hyps, Concl, Seqs) :-
	findall(Seq, equcons2seq(Hyps, Concl, Seq), Seqs0),
	remove_axioms(Seqs0, Seqs1),
	minimize_seqs(Seqs1, Seqs).

equcons2seq(Hyps, A=B, Seq) :-
	truth_values(TVs),
	eq2seq(A, B, TVs, CSeq),
	neq2seq_l(Hyps, TVs, HSeqs),
	ms_flatten([CSeq|HSeqs], Seq0),
	sort(Seq0, Seq).

% Returns one of the sequents that characterize the equation A=B
% To find all, use backtracking.
eq2seq(A, B, TVs0, [A^TV|SeqB]) :-
	ms_select(TVs0, TV, TVs1),
	ss_sas(SeqB, [B^TVs1]).

% Returns one of the sequents that characterize the negation
% of the equation A=B
% To find all, use backtracking.
neq2seq(A, B, TVs0, Seq) :-
	ms_select(TVs0, _TV, TVs1),
	ss_sas(SeqA, [A^TVs1]),
	ss_sas(SeqB, [B^TVs1]),
	ms_append(SeqA, SeqB, Seq).

neq2seq_l([], _TVs, []).
neq2seq_l([A=B|Ns], TVs, [S|Ss]) :-
	neq2seq(A, B, TVs, S),
	neq2seq_l(Ns, TVs, Ss).

% counterexample(Seq, Cex)
%   <Cex> is a counterexample falsifying <Seq>
% counterexamples(Seq, Cexs)
%   <Cexs> is the list of all counterexamples of <Seq>
% counterexample_l(Seqs, Cex)
%   <Cex> is a counterexample falsifying some sequent in <Seqs>
% counterexamples_l(Seqs, Cexs)
%   <Cexs> is the list of all counterexamples falsifying
%   some sequent in <Seqs>.

counterexample(Seq0, Cex) :-
	ss_sas(Seq0, Seq1),
	truth_values(TVs),
	counterexample(Seq1, TVs, Cex).

counterexample([], _TVs, []).
counterexample([A^Ss|Ls0], TVs, [A^S|Ls]) :-
	diff(TVs, Ss, CSs),
	member(S, CSs),
	counterexample(Ls0, TVs, Ls).

counterexamples(Seq, Cexs) :-
	findall(Cex, counterexample(Seq, Cex), Cexs).

counterexample_l(Seqs, Cex) :-
	member(Seq, Seqs),
	counterexample(Seq, Cex).

counterexamples_l(Seqs, Cexs) :-
	findall(Cex, counterexample_l(Seqs, Cex), Cexs0),
	sort(Cexs0, Cexs1),
	minimize_seqs(Cexs1, Cexs).
