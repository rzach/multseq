% hypotheses(Sequent, Hypotheses)
%   generate the list of unprovable Hypotheses of Sequent
%
% hypotheses_l(Sequents, Hypotheses)
%   generate the list of unprovable Hypotheses of a list of Sequents

hypotheses(Sequent, Hypotheses) :-
    hypotheses(Sequent, [[]], Hypotheses).

hypotheses([], Hypotheses, Hypotheses).
hypotheses([Literal|Sequent], Hypotheses0, Hypotheses) :-
    hypotheses_literal(Literal, Hypotheses1),
    findall(Hypothesis,
            new_hypothesis(Hypotheses0, Hypotheses1, Hypothesis),
            Hypotheses2
           ),
    sort(Hypotheses2, Hypotheses3),
    minimize_seqs(Hypotheses3, Hypotheses4),
    hypotheses(Sequent, Hypotheses4, Hypotheses).

hypotheses_literal(Literal, Hypotheses) :-
    rule(Literal, Premises0, _Name),
    !,
    maplist(sort, Premises0, Premises1),
    hypotheses_l(Premises1, Hypotheses).
hypotheses_literal(Literal, [[Literal]]).

new_hypothesis(Hs1, Hs2, H) :-
    member(H1, Hs1),
    member(H2, Hs2),
    append(H1, H2, H1H2),
    sort(H1H2, H),
    \+ contains_axiom(H, _A).

hypotheses_l(Sequents, Hypotheses) :-
    hypotheses_l(Sequents, [], Hypotheses0),
    sort(Hypotheses0, Hypotheses).

hypotheses_l([], Hypotheses, Hypotheses).
hypotheses_l([Sequent|Sequents], Hypotheses0, Hypotheses) :-
    hypotheses(Sequent, Hypotheses1),
    append(Hypotheses0, Hypotheses1, Hypotheses2),
    minimize_seqs(Hypotheses2, Hypotheses3),
    hypotheses_l(Sequents, Hypotheses3, Hypotheses).

% contains_axiom(Sequent, A).
% Checks whether <Sequent> contains axiom "<A>|...|<A>".
% Depends on the fact that truth values and sequents are kept sorted.
%
contains_axiom(Seq0, A) :-
    ss_sas(Seq0, Seq1),
    truth_values(TVs),
    member(A^TVs, Seq1).

is_provable(S) :-
    hypotheses(S, []).

% derivable(Sequent, Derivation).
%    <Derivation> is a derivation of <Sequent>.
%    A hypothesis is introduced if no rule is applicable.

derivable(S, ra(ax(A),S,[])) :-
    contains_axiom(S, A),
    !.
derivable(S, ra(R,S,Ts)) :-
    option(strategy(Strategy)),
    select_rule(Strategy, S, Sl, Sr, Ps0, R),
    !,
    appseqs(Ps0, Sl, Sr, Ps1),
    maplist(derivable, Ps1, Ts).
derivable(S, ra(hyp,S,[])).

derivable_l([], []).
derivable_l([P|Ps], [T|Ts]) :-
    derivable(P, T),
    derivable_l(Ps, Ts).

% provable(Sequent, Proof).
%    <Proof> is a proof of <Sequent>.
% The same as "derivable(Sequent, Proof)", except that "provable"
% fails if a leaf is not an axiom, while "derivable" inserts
% a hypothesis.

provable(S, ra(ax(A),S,[])) :-
    contains_axiom(S, A),
    !.
provable(S, ra(R,S,Ts)) :-
    option(strategy(Strategy)),
    select_rule(Strategy, S, Sl, Sr, Ps0, R),
    !,
    appseqs(Ps0, Sl, Sr, Ps1),
    maplist(provable, Ps1, Ts).

provable_l([], []).
provable_l([P|Ps], [T|Ts]) :-
    provable(P, T),
    provable_l(Ps, Ts).

% is_provable(Sequent).
% The same as "provable(Sequent, _)."

% is_provable(S) :-
%     contains_axiom(S, _),
%     !.
% is_provable(S) :-
%     option(strategy(Strategy)),
%     select_rule(Strategy, S, Sl, Sr, Ps0, _R),
%     !,
%     appseqs(Ps0, Sl, Sr, Ps1),
%     maplist(is_provable, Ps1).

% provable_all(Sequent, Proof).
%    <Proof> is a proof of <Sequent>.
% The same as provable(Sequent, Proof), except that provable_all
% enumerates all possible proofs.
% NOTE: if there is no proof, provable_all will explore all permutations
% of rule applications before failing, which may take a long time.

provable_all(S, ra(ax(A),S,[])) :-
    contains_axiom(S, A),
    !.
provable_all(S, ra(R,S,Ts)) :-
    option(strategy(Strategy)),
    select_rule(Strategy, S, Sl, Sr, Ps0, R),
    appseqs(Ps0, Sl, Sr, Ps1),
    maplist(provable_all, Ps1, Ts).

% select_rule(Strategy, Sequent, Sequent_l, Sequent_r, Premises, Rulename).
%    Select a formula F in <Sequent> and a rule <Rulename> using <Strategy>.
%    <Sequent_l> and <Sequent_r> are the parts of <Sequent> to the left
%    and right of F, respectively.
%    <Premises> are the premises of the rule instance that yields F.

select_rule(interactive, S, Sl, Sr, Ps, R) :-
    findall(pos_rule(Sl,F,Sr,Ps,R),
            (split(S, Sl, F, Sr),rule(F, Ps, R)),
            Pos_rules),
    choose_rule(S, Pos_rules, Sl, Sr, Ps, R).
select_rule(leftright, S, Sl, Sr, Ps, R) :-
    split(S, Sl, F, Sr),
    rule(F, Ps, R).
select_rule(topdown, S, Sl, Sr, Ps, R) :-
    rule(F, Ps, R),
    split(S, Sl, F, Sr).
select_rule(ordering(Rs), S, Sl, Sr, Ps, R) :-
    ms_member(R,Rs),
    rule(F, Ps, R),
    split(S, Sl, F, Sr).

choose_rule(_S, [pos_rule(Sl,_F,Sr,Ps,R)], Sl, Sr, Ps, R).
choose_rule(S, Pos_rules, Sl, Sr, Ps, R) :-
    Pos_rules = [_,_|_],
    ms_writeln,
    ms_writeln(['Sequent: ', S]),
    display_rules(Pos_rules, 0, N),
    get_rule_number(N, I),
    nth(I, Pos_rules, pos_rule(Sl, _F, Sr, Ps, R)).

display_rules([], N, N).
display_rules([pos_rule(Sl,_F,Sr,Ps,R)|Rs], I0, N) :-
    I1 is I0 + 1,
    display_rule(I1, Sl, Sr, Ps, R),
    display_rules(Rs, I1, N).

display_rule(I, Sl, Sr, Ps, R) :-
    ms_writeln([I, ': rule ', R, ' yields the premise(s)']),
    display_premises(Ps, Sl, Sr).

display_premises([], _Sl, _Sr).
display_premises([P|Ps], Sl, Sr) :-
    ms_append(P, Sr, PSr),
    ms_append(Sl, PSr, SlPSr),
    ms_writeln(['   ', SlPSr]),
    display_premises(Ps, Sl, Sr).

get_rule_number(N, I) :-
    repeat,
    writel(['Your choice: ']), read(I),
    1 =< I, I =< N,
    !.

appseqs([], _Sl, _Sr, []).
appseqs([S0|Ss0], Sl, Sr, [S|Ss]) :-
    ms_append(Sl,S0,S1),
    ms_append(S1,Sr,S2),
    sort(S2,S),
    appseqs(Ss0, Sl, Sr, Ss).

remove_axioms([], []).
remove_axioms([S|Ss], Ts) :-
    contains_axiom(S, _A),
    !,
    remove_axioms(Ss, Ts).
remove_axioms([S|Ss], [S|Ts]) :-
    remove_axioms(Ss, Ts).

% collect_hyps(ProofTree, Hypotheses)
%    Gathers all hypotheses occurring in <ProofTree>
%    and returns in <Hypotheses> a minimized list thereof.

collect_hyps(T, Hs) :-
    collect_hyps_l([T], Hs).

collect_hyps_l(Ts, Hs) :-
    collect_hyps_l(Ts, Hs0, []),
    minimize_seqs(Hs0, Hs).

collect_hyps_l([], Hs, Hs).
collect_hyps_l([T|Ts], Hs0, Hs) :-
    collect_hyps(T, Hs0, Hs1),
    collect_hyps_l(Ts, Hs1, Hs).

collect_hyps(ra(hyp,S,[]), [S|Hs], Hs).
collect_hyps(ra(R, _, Ts), Hs0, Hs) :-
    R \= hyp,
    collect_hyps_l(Ts, Hs0, Hs).

minimize_seqs([], []).
minimize_seqs([H|Hs0], Hs) :-
    subsequent_hs_h(Hs0, H), !,
    minimize_seqs(Hs0, Hs).
minimize_seqs([H|Hs0], [H|Hs]) :-
    subsequent_h_hs(Hs0, H, Hs1),
    minimize_seqs(Hs1, Hs).

subsequent_hs_h([G|_], H) :-
    subsequent(G, H).
subsequent_hs_h([_|Gs], H) :-
    subsequent_hs_h(Gs, H).

subsequent_h_hs([], _G, []).
subsequent_h_hs([H|Hs0], G, Hs) :-
    subsequent(G, H), !,
    subsequent_h_hs(Hs0, G, Hs).
subsequent_h_hs([H|Hs0], G, [H|Hs]) :-
    subsequent_h_hs(Hs0, G, Hs).

subsequent([], _).
subsequent([A|As], [A|Bs]) :- !,
    subsequent(As, Bs).
subsequent(As, [_|Bs]) :-
    subsequent(As, Bs).


% proof_skeleton(ProofTree, Skeleton, SeqDictionary)
%   Replaces in <ProofTree> all sequents by numbers
%   giving <Skeleton>; <SeqDictionary> is list of
%   pairs Number:Sequent relating sequents to numbers.

proof_skeleton(P, Sk, Sqs) :-
    proof_skeleton(P, Sk, [], Sqs0, 0, _I),
    ms_reverse(Sqs0, Sqs).

proof_skeleton(ra(R,Sq,Ps), ra(R,N,Sks), Sqs0, Sqs, I0, I) :-
    ms_member(N:Sq, Sqs0), !,
    proof_skeleton_l(Ps, Sks, Sqs0, Sqs, I0, I).
proof_skeleton(ra(R,Sq,Ps), ra(R, I1, Sks), Sqs0, Sqs, I0, I) :-
    I1 is I0 + 1,
    proof_skeleton_l(Ps, Sks, [I1:Sq|Sqs0], Sqs, I1, I).

proof_skeleton_l([], [], Sqs, Sqs, I, I).
proof_skeleton_l([P|Ps], [Sk|Sks], Sqs0, Sqs, I0, I) :-
    proof_skeleton(P, Sk, Sqs0, Sqs1, I0, I1),
    proof_skeleton_l(Ps, Sks, Sqs1, Sqs, I1, I).

