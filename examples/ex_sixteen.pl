% Test file to check things in SIXTEEN

% make sure MUltseq is loaded
:- ensure_loaded('../multseq/multseq').

% load sample properties
:- [properties].

% load the rules
:- load_logic('shramko-wansing.msq').

% define standard Omap
:- setOmap([(negt)/(-),andt/(/\),ort/(\/)]). 

% define generators

imp_ops(ort(negt(X),Y)/[X,Y]).
imp_ops(orf(negf(X),Y)/[X,Y]).
imp_ops(ort(negf(X),Y)/[X,Y]).
imp_ops(orf(negt(X),Y)/[X,Y]).

or_ops(ort(X,Y)/[X,Y]).
or_ops(orf(X,Y)/[X,Y]).

and_ops(andt(X,Y)/[X,Y]).
and_ops(andf(X,Y)/[X,Y]).

neg_ops(negt(X)/[X]).
neg_ops(negf(X)/[X]).

% auxiliary predicate callall

callall([]) :- !.
callall([P|Ps]) :-
  (call(P) ->
    callall(Ps)
  ; callall(Ps)).

% check all properties and write report to out.tex

:- set_option(tex_output(terse)).

:- start_logging(ex_sixteen,'.tex').

:- print_tex(tex_title("Experiments in SIXTEEN")).

:- print_tex(tex_logic).

:- print_tex(tex_section(["Finding an implication in SIXTEEN"])).

% check if any combination of -a \/ b satisfies modus ponens and
% deduction theorem

:- print_tex(tex_paragraph(["In classical logic, implication can be defined as $\\lnot A \\lor B$. But SIXTEEN has two versions of $\\lnot$ and two versions of $\\lor$. For all resulting combinations, we check if the equivalents of modus ponens and the deduction theorem are valid."])).

:-  (property(modusponens, _, P), 
      property(deductionthm, _, Q),
      instantiate(> : [P,Q] @ imp_ops, PP),
      callall(PP), 
      fail)
    ; true.

:- print_tex(tex_section(["Checking De Morgan Triples"])).

% check all combinations of De Morgan's laws (as quasi-equations)

:- print_tex(tex_paragraph(["In classical logic, $\\lnot$, $\\lor$, and $\\land$ satisfy De Morgan's laws. But SIXTEEN has two versions of each. For all resulting combinations, we check if the corresponding De Morgan laws are valid."])).


:-  ( property(demorganor, _, P),
      property(demorganand, _, Q),
      instantiate([\/, /\, -] : [P,Q] @ [or_ops, and_ops, neg_ops], PP),
      callall(PP),
      fail)
    ; true.

:- print_tex(tex_listing("ex_sixteen.pl")).

:- stop_logging.