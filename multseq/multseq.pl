%            __  __ _   _ _ _
%           |  \/  | | | | | |_ ___  ___  __ _
%           | |\/| | | | | | __/ __|/ _ \/ _` |
%           | |  | | |_| | | |_\__ \  __/ (_| |
%           |_|  |_|\___/|_|\__|___/\___|\__, |
%                                           |_|
%
% A generic sequent prover for propositional many-valued logics,
% and much more ...

:- set_prolog_flag(double_quotes, codes).

:- [msconf].    % OS- and Prolog-specific settings; currently only end-of-line
:- [msutil].    % auxiliary predicates
:- [mscalcul].  % proof construction, proof transformations, ...
:- [msconseq].  % consequence relations on sequents, formulas, equations
:- [mslgcin].   % input of logic specification
:- [msoption].  % option processing
:- [mstex].     % output routines (TeX)
:- [msschema].  % handle formula schemes
:- [mslogging]. % code for logging

% defaultOmap(Omap).
% Omap binds to the default list of Op1/Op2 pairs used by chkProp/1.

:- dynamic defaultOmap/1.

% setOmap(Omap)
% set the default operator map to Omap

setOmap(Omap) :-
    retractall(defaultOmap(_)),
    assert(defaultOmap(Omap)).

% DEPRECATED
% chkPropOld -- test if the property with name Prop holds for the logic
% (using the operator/arity names Ops)
% e.g., chkProp(modustollens, [imp/2, neg/1]).
% The validity of the "logical" properties (not the algebraic
% ones) depend on the designated truth values that are read from ".msq"
% but I think that it would be better to use an extra argument here
% chkProp(Prop, Ops, TVs) so one can check if the propoerty holds for
% other designated truth_values

chkPropOld(Prop, Ops) :-
    property(Prop, SOps, Specs),
    pairOps(Ops, SOps, Omap),
    replaceOps(Omap, Specs, RSpecs),
    call(RSpecs).

% chkProp -- test if the property with name Prop holds for the logic
% (using the operator map Omap)
% e.g., chkOmapProp([imp/(>), neg/(-)], modustollens).
% (Omap goes first so you can do maplist(chkPropOmap(Omap), [prop1,
% prop2, prop3]).

chkProp(Prop) :-
    defaultOmap(Omap), !,
    chkProp(Omap, Prop).

chkProp(Omap, Prop) :-
    property(Prop, _, Spec),
    replaceOps(Omap, Spec, RSpec),
    print_message(informational, ms_prop_check(RSpec)),
    call(RSpec).

% compareProp(Rep, Prop)
% Rep is a list of Rs/O pairs, where Rs is a list of operators to
% replace O by. Runs chkProp on property Prop for all combinations of
% replacements. Note that it fails if none of the replaced operators
% occurs in Prop. Operators not mentioned in Rep get replaced by the
% default ones for defaultOmap.

compareProp(Rep,Prop) :-
    property(Prop,Ops,_),
    bagof(X,A^member(X/A,Ops),OccOps),
    bagof(R/X,(member(X,OccOps), member(R/X,Rep)), RepOps),
    defaultOmap(DefOmap),
    maplist(selectOp, RepOps, Omap2),
    append(Omap2,DefOmap,Omap),
    chkProp(Omap,Prop).
selectOp(Ops/X,Op/X):-
    member(Op,Ops).

property_schema(Prop, Schema) :-
    property(Prop, SOps, Specs),
    property_schema(SOps, Specs, Schema).

property_schema([], Schema, Schema).
property_schema([F/A|SOps], Spec, Schema) :-
    property_schema(SOps, F:Spec@operators(A), Schema).

% Evaluate actual specifications of properties. Can also be used
% directly, e.g., if the logic contains operators or and neg,
% tautology(or(neg(a),a)) will succeed if (neg(a) or a) is a tautology.

% tautology(Fmla)
% Is Fmla a tautology?

tautology(Fmla) :-
    frmcons2seq([] => Fmla, Seq),
    print_message(informational, ms_proof(Seq)),
    (option(tex_output(verbose)) ->
        derivable(Seq, Proof), !,
        collect_hyps(Proof, Hs),
        counterexamples_l(Hs, Cexs),
        (Cexs = [] ->
            (option(tex_success(on)) -> print_tex(tex_report(tautology, Fmla)),
            print_tex(tex_report(evidence, [Seq], Cexs, [Proof])))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(nottautology, Fmla)),
                print_tex(tex_report(evidence, [Seq], Cexs, [Proof]))),
            fail)
    ;   (is_provable(Seq) ->
            (option(tex_success(on)) -> print_tex(tex_report(tautology, Fmla)))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(nottautology, Fmla))),
            fail)
    ).

% consequence(Prems, Concl)
% Do Prems entail Concl?

consequence(Prems => Concl) :-
    frmcons2seq(Prems => Concl, Seq), % turn consequence statement into sequent
    print_message(informational, ms_proof(Seq)),
    (option(tex_output(verbose)) ->
        derivable(Seq, Proof),
        collect_hyps(Proof, Hs),
        counterexamples_l(Hs, Cexs),
        !,
        (Cexs = [] ->
            (option(tex_success(on)) -> print_tex(tex_report(consequence, Prems, Concl)),
                print_tex(tex_report(evidence, [Seq], Cexs, [Proof])))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(notconsequence, Prems, Concl)),
                print_tex(tex_report(evidence, [Seq], Cexs, [Proof]))),
            fail)
    ;   (is_provable(Seq) ->
            (option(tex_success(on)) -> print_tex(tex_report(consequence, Prems, Concl)))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(notconsequence, Prems, Concl))),
            fail)
    ).

% equivalence(Fmla1, Fmla2)
% Are Fmla1 and Fmla2 logically equivalent?
% Note that we test both directions.

equivalence(Fmla1, Fmla2) :-
    frmcons2seq([Fmla1] => Fmla2, Seq1),
    frmcons2seq([Fmla2] => Fmla1, Seq2),
    print_message(informational, ms_proof(Seq1)),
    print_message(informational, ms_proof(Seq2)),
    (option(tex_output(verbose)) ->
        derivable_l([Seq1,Seq2], Proofs),
        collect_hyps_l(Proofs, Hs),
        counterexamples_l(Hs, Cexs),
        !,
        (Cexs = [] ->
            (option(tex_success(on)) -> print_tex(tex_report(equivalent, Fmla1, Fmla2)),
                print_tex(tex_report(evidence, [Seq1, Seq2], Cexs, Proofs)))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(notequivalent, Fmla1, Fmla2)),
                print_tex(tex_report(evidence, [Seq1, Seq2], Cexs, Proofs))),
            fail)
    ;   ((is_provable(Seq1), is_provable(Seq2)) ->
            (option(tex_success(on)) -> print_tex(tex_report(equivalent, Fmla1, Fmla2)))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(notequivalent, Fmla1, Fmla2))),
            fail)
    ).

% equality(Fmla1, Fmla2)
% Are Fmla1 and Fmla2 equationally equal?

equality(Fmla1, Fmla2) :-
    % equcons2seqs([], Fmla1 = Fmla2, Seqs), % turn equation into list of sequents
    equcons2seqs([], Fmla1 = Fmla2, Seqs),
    forall(member(Seq, Seqs), print_message(informational, ms_proof(Seq))),
    (option(tex_output(verbose)) ->
        derivable_l(Seqs, Proofs),
        collect_hyps_l(Proofs, Hs),
        counterexamples_l(Hs, Cexs),
        !,
        (Cexs = [] ->
            (option(tex_success(on)) -> print_tex(tex_report(equal, Fmla1, Fmla2)),
                print_tex(tex_report(evidence, Seqs, Cexs, Proofs)))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(notequal, Fmla1, Fmla2)),
                print_tex(tex_report(evidence, Seqs, Cexs, Proofs))),
            fail)
    ;   (forall(member(Seq, Seqs), is_provable(Seq)) ->
            (option(tex_success(on)) -> print_tex(tex_report(equal, Fmla1, Fmla2)))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(notequal, Fmla1, Fmla2))),
            fail)
    ).

% metaconseq(Hyps, Cons)
% Do the consequence statements Hyps meta-imply Cons?

metaconseq(Hyps, Cons) :-
    maplist(frmcons2seq, Hyps, HSeqs),
    frmcons2seq(Cons, CSeq),
    seqcons2seqs(HSeqs, CSeq, Seqs),
    forall(member(Seq, Seqs), print_message(informational, ms_proof(Seq))),
    (option(tex_output(verbose)) ->
        derivable_l(Seqs, Proofs),
        collect_hyps_l(Proofs, Hs),
        counterexamples_l(Hs, Cexs), 
        !,
        (Cexs = [] ->
            (option(tex_success(on)) -> print_tex(tex_report(metaconseq, Hyps, Cons)),
                print_tex(tex_report(evidence, Seqs, Cexs, Proofs)))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(notmetaconseq, Hyps, Cons)),
                print_tex(tex_report(evidence, Seqs, Cexs, Proofs))),
            fail)
    ;   (forall(member(Seq, Seqs), is_provable(Seq)) ->
            (option(tex_success(on)) -> print_tex(tex_report(metaconseq, Hyps, Cons)))
        ;   (option(tex_failure(on)) -> print_tex(tex_report(notmetaconseq, Hyps, Cons))),
            fail)
    ).

% pairOps -- turn two Operator/Arity lists into an operator map, i.e.,
% a list of Operator pairs, e.g., [and/2, imp/2] and [(&)/2, (>)/2]
% -> [and/&, or/(>)]

pairOps([], [], []) :- !.
pairOps([Op/A | Ops], [SOp/A | SOps], [Op/SOp | ROps]) :-
    pairOps(Ops, SOps, ROps).

% replaceOps -- takes a list of operator pairs and replaces every
% occurrence of the second in a pair in SFmla by the first, e.g.,
% [and/&, imp/(>)], (a & b) > c  -> imp(and(a, b), c)

replaceOps(Omap, SFmla, Fmla) :-
    SFmla =.. [SOp|SArgs],
    maplist(replaceOps(Omap), SArgs, Args),
    (member(Op/SOp, Omap) ->
        Fmla =.. [Op|Args]
    ; Fmla =.. [SOp|Args]).

%%% INFORMATIONAL MESSAGES

prolog:message(ms_prop_check(Spec)) -->
    [ 'Checking specification ~w'-[Spec] ].
prolog:message(ms_proof(Seq)) -->
    [ 'Proving sequent ~w'-[Seq] ].

%%% LOGGING MESSAGES

:- multifile user:message_property/2.

user:message_property(log, color([fg(blue)])).

% logging_intro :-
%    Code to be executed by start_logging.
%    session_log is a file handle pointing to the session log.

logging_intro :-
    set_stream(session_log, alias(tex_output)),
    ( option(tex_output(off)) ; print_tex(tex_start) ).

% logging_outro :-
%    Code to be executed when logging is stopped, either by
%    stop_logging or by halting the interactive interpreter.

logging_outro :-
    ( option(tex_output(off)) ; print_tex(tex_stop) ),
    set_stream(user_error, alias(tex_output)).

% Uncomment to start logging automatically when loading MUltseq.
%:- start_logging.
