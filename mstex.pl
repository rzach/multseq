tex_title(T) -->
	bslash, "title{", T, "}", eol,
	bslash, "author{M.~Ultseq}", eol,
	bslash, "maketitle", eol,
	eol.

tex_section(T) -->
	bslash, "section{", T, "}", eol,
	eol.

%tex_report(Kind, Id, Hyps, Conc, Seqs, Cexs, Extras).
tex_report(seqcons, Id, [], S, _, Cexs, Extras) -->
	tex_example_heading(Id),
	tex_problem(["Is the sequent $",tex_seq(S),"$ provable?"]),
	({Cexs=[]} ->
	   tex_answer(["Yes, it is."]),
	   ({member(deriv(P), Extras)} -> tex_derivation(["Proof"],S,P); true),
	   ({member(skel(Sk,Sqs), Extras)} -> tex_skel("Proof",S,Sk,Sqs); true)
	;  tex_answer(["No, it is not."]),
	   ({member(deriv(P), Extras)} -> tex_derivation(["Derivation"],S,P); true),
	   ({member(hyps(Hs), Extras)} -> tex_hypotheses(Hs); true),
	   ({member(skel(Sk,Sqs), Extras)} -> tex_skel("Derivation",S,Sk,Sqs); true),
	   tex_counter(Cexs)
	).
tex_report(seqcons, Id, Hyps, Conc, Seqs, Cexs, _Extras) -->
	{Hyps = [_|_]},
	tex_example_heading(Id),
	tex_problem(["Is the consequence relation ",tex_seqcons(Hyps,Conc)," valid?"]),
	tex_seqproblem(Seqs),
	({Cexs=[]} ->
	   tex_answer(["Yes, the consequence relation holds."])
	;  tex_answer(["No, the consequence relation does not hold."]),
	   tex_counter(Cexs)
	).
tex_report(frmcons(DTVs), Id, [], F, Seqs, Cexs, Extras) -->
	tex_example_heading(Id),
	tex_problem(["Let $", tex_tvs(DTVs), "$ be the set of designated truth values. ",
                     "Is the formula $",tex_mvf(F),"$ valid?"]),
	tex_seqproblem(Seqs),
	({Cexs=[]} ->
	   tex_answer(["Yes, the formula is valid."]),
	   ({member(deriv(P), Extras)} ->
	      Seqs=[S],
	      tex_derivation(["Proof"],S,P)
	   ;  true
	   )
	;  tex_answer(["No, the formula is not valid."]),
	   tex_counter(Cexs)
	).
tex_report(frmcons(DTVs), Id, Hyps, Conc, Seqs, Cexs, Extras) -->
	{Hyps = [_|_]},
	tex_example_heading(Id),
	tex_problem(["Let $", tex_tvs(DTVs), "$ be the set of designated truth values. ",
                     "Is the consequence relation ",tex_frmcons(Hyps,Conc)," valid?"]),
	tex_seqproblem(Seqs),
	({Cexs=[]} ->
	   tex_answer(["Yes, the consequence relation holds."]),
	   ({member(deriv(P), Extras)} ->
	      Seqs=[S],
	      tex_derivation(["Proof"],S,P)
	   ;  true
	   )
	;  tex_answer(["No, the consequence relation does not hold."]),
	   tex_counter(Cexs)
	).
tex_report(equcons, Id, [], E, Seqs, Cexs, _Extras) -->
	tex_example_heading(Id),
	tex_problem(["Is the equation $",tex_equ(E),"$ valid?"]),
	tex_seqproblem(Seqs),
	({Cexs=[]} ->
	   tex_answer(["Yes, the equation is valid."])
	;  tex_answer(["No, the equation is not valid."]),
	   tex_counter(Cexs)
	).
tex_report(equcons, Id, Hyps, Conc, Seqs, Cexs, _Extras) -->
	{Hyps = [_|_]},
	tex_example_heading(Id),
	tex_problem(["Is the quasi-equation ",tex_equcons(Hyps, Conc)," valid?"]),
	tex_seqproblem(Seqs),
	({Cexs=[]} ->
	   tex_answer(["Yes, the quasi-equation is valid."])
	;  tex_answer(["No, the quasi-equation is not valid."]),
	   tex_counter(Cexs)
	).


tex_example_heading(N) -->
	{name(N, NS)},
	bslash, "subsection{Example ", NS, "}", eol,
	eol.

tex_problem(P) -->
	tex_environment("problem", P).

tex_answer(A) -->
	tex_environment("answer", A).

tex_skel(Kind, S, Sk, Sqs) -->
	{option(tex_proofstyle(Old)),
	 set_option(tex_proofstyle(bare))
	},
	tex_derivation([Kind," skeleton"], S, Sk),
	{set_option(tex_proofstyle(Old))},
	tex_seqlist(Sqs, ["Table of sequents:"]).
	
tex_hypotheses(Hs) -->
	tex_seqlist(Hs, ["List of hypotheses:"]).

tex_counter(Cex) -->
	tex_seqlist(Cex, ["List of counter-examples:"]).

tex_environment(Kind, Contents) -->
	bslash, "begin{", Kind, "}", eol,
	phrase_list(Contents), eol,
	bslash, "end{", Kind, "}", eol.

tex_seqproblem(Seqs) -->
	{(Seqs = [_] -> EndS = ""; EndS = "s")},
	tex_seqlist(Seqs,
	   ["The problem is equivalent to proving the following sequent", 
           EndS, ":"]).

tex_seqcons(Hyps, Concl) -->
	tex_cons(tex_seq, Hyps, Concl).

tex_frmcons(Hyps, Concl) -->
	tex_cons(tex_mvf, Hyps, Concl).

tex_equcons(Hyps, Concl) -->
	tex_cons(tex_equ, Hyps, Concl).

tex_cons(Tex_item, Hyps, Concl) -->
	bslash, "[", eol,
	tex_cons1(Hyps, Tex_item),
	bslash, "vdash ",
	tex_cons1([Concl], Tex_item),
	bslash, "]", eol.

tex_cons1([], _) -->
	[].
tex_cons1([I], Tex_item) -->
	{X =.. [Tex_item, I]},
	X.
tex_cons1([I|Is], Tex_item) -->
	{Is = [_|_]},
	{X =.. [Tex_item,I]},
	X,
	", ", eol,
	tex_cons1(Is, Tex_item).

tex_seqlist([],_Headline) -->
	[].
tex_seqlist(Hs,Headline) -->
	{Hs = [S|_]},
	phrase_list(Headline), eol,
	bslash, "[", bslash, "begin{array}{",
	({S = _:_} -> "r"; {true}),
	"l}", eol,
	tex_seqlist1(Hs),
	"  ", bslash, "end{array}", eol,
	bslash, "]", eol.

tex_seqlist1([H]) -->
	tex_seq(H), eol.
tex_seqlist1([H|Hs]) -->
	{Hs = [_|_]},
	tex_seq(H), bslash, bslash, bslash, "relax",  eol,
	tex_seqlist1(Hs).

tex_derivation(Kind, S, PT) -->
	phrase_list(Kind), " of $", tex_seq(S), "$:", eol,
	bslash, "[", eol,
	tex_derivation1(PT),
	bslash, "]", eol.

tex_derivation1(ra(ax(A), S, _)) -->
	tex_ax(A, S).
tex_derivation1(ra(hyp, S, _)) -->
	tex_hyp(S).
tex_derivation1(ra(R, S, PTs)) -->
	tex_rule_app(R, S, PTs).

tex_ax(A, S) -->
	bslash, "deduce{", tex_seq(S), "}%", eol,
        "{", tex_ax1(A), "}", eol.

tex_ax1(A) -->
	{option(tex_proofstyle(verbose))},
	bslash, "mbox{axiom for $", tex_mvf(A), "$}".
tex_ax1(A) -->
	{option(tex_proofstyle(compact))},
	bslash, "mathit{ax}(", tex_mvf(A), ")".
tex_ax1(_A) -->
	{option(tex_proofstyle(bare))}.

tex_hyp(S) -->
	bslash, "deduce{", tex_seq(S), "}%", eol,
        "{", tex_hyp1, "}", eol.

tex_hyp1 -->
	{option(tex_proofstyle(verbose))},
	bslash, "mbox{hypothesis}".
tex_hyp1 -->
	{option(tex_proofstyle(compact))},
	bslash, "mathit{hyp}".
tex_hyp1 -->
	{option(tex_proofstyle(bare))}.

tex_rule_app(R, S, PTs) -->
	bslash, "infer", tex_rule_name(R), "{", tex_seq(S), "}%", eol,
        "{", tex_premises(PTs), "}", eol.

tex_rule_name(R) -->
	{option(tex_rulenames(on))},
	{tex_rn(R, RT)},
	"[", phrase_list(RT), "]".
tex_rule_name(R) -->
	{option(tex_rulenames(on))},
	{\+ tex_rn(R, _)},
	{name(R, RS)},
	"[", bslash, "mathit{", RS, "}]".
tex_rule_name(_) -->
	{option(tex_rulenames(off))}.

tex_premises([]) -->
	[].
tex_premises([PT]) -->
	tex_derivation1(PT).
tex_premises([PT|PTs]) -->
	{PTs = [_|_]},
	tex_derivation1(PT), "&", eol,
	tex_premises(PTs).

tex_seq(N:S) -->
	!,
	{name(N, NS)},
	NS, "{:} & ", tex_seq1(S).
tex_seq(S) -->
	tex_seq1(S).

tex_seq1(S) -->
	{number(S), name(S,SS)}, !,
	SS.
tex_seq1(S) -->
	{option(tex_sequents(signed))},
	"[", tex_seq_s(S), "]".
tex_seq1(S) -->
	{option(tex_sequents(multidimensional))},
	"[", tex_seq_m(S), "]".

tex_seq_s([]) -->
	[].
tex_seq_s([F]) -->
	tex_sf(F).
tex_seq_s([F|S]) -->
	{S = [_|_]},
	tex_sf(F),
	", ",
	tex_seq_s(S).

tex_seq_m(S) -->
	{truth_values_orig(TVs)},
	tex_seq_m1(TVs, S).

tex_seq_m1([TV|TVs], S) -->
	{collect(S, TV, Fs)},
	tex_mvfs(Fs),
	tex_seq_m2(TVs, S).

tex_seq_m2([], _) -->
	[].
tex_seq_m2(TVs, S) -->
	{TVs = [_|_]},
	" ", bslash, "mid ",
	tex_seq_m1(TVs, S).

collect([], _, []).
collect([F^TV|S], TV, [F|Fs]) :-
	!,
	collect(S, TV, Fs).
collect([_|S], TV, Fs) :-
	collect(S, TV, Fs).

tex_mvfs([]) -->
	bslash, "emptyset".
tex_mvfs([F]) -->
	tex_mvf(F).
tex_mvfs([F|Fs]) -->
	{Fs = [_|_]},
	tex_mvf(F),
	", ",
	tex_mvfs(Fs).

tex_sf(F^S) -->
	tex_mvf(F),
	"^",
	tex_tv(S).

tex_tvs(Ss) -->
	bslash, "{",
	tex_tvs1(Ss),
	bslash, "}".

tex_tvs1([]) -->
	[].
tex_tvs1([S]) -->
	tex_tv(S).
tex_tvs1([S|Ss]) -->
	{Ss = [_|_]},
	tex_tv(S), ",",
	tex_tvs1(Ss).

tex_tv(S) -->
	{tex_tv(S, ST)},
	!,
	phrase_list(ST).
tex_tv(S) -->
	{name(S, ST)},
	bslash, "mathit{", ST, "}".

tex_equ(L=R) -->
	tex_mvf(L), "=", tex_mvf(R).

tex_mvf(F0) -->
	{functor(F0, Op, N),
	 functor(F1, Op, N),
	 tex_op(F1, FTeX)},
	!,
	{F0 =.. [_|Args0],
	 add_tex_mvf(Args0, Args1),
	 F1 =.. [_|Args1]},
	phrase_list(FTeX).
tex_mvf(F) -->
	{F =.. [Op|Args]},
	tex_conn(Op),
	tex_args(Args).

tex_conn(Op) -->
	{tex_opname(Op, OpT)},
	!,
	phrase_list(OpT).
tex_conn(Op) -->
	{name(Op, OpT)},
	bslash, "mathit{", OpT, "}".

tex_args([]) -->
	[].
tex_args(Fs) -->
	"(",
	tex_mvfs(Fs),
	")".

add_tex_mvf([], []).
add_tex_mvf([F|Fs0], [tex_mvf(F)|Fs]) :-
	add_tex_mvf(Fs0, Fs).

bslash --> "\\".

true -->
	[].
