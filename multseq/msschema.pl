:- op(600, yfx, @).
%:- op(600, xfy, :). % pre-defined, better not touch

% instantiate(+Schema, -Formula)
% Generate a formula from Schema by replacing operators
% named by in a ':'-prefix by formulas generated by the
% generators mentioned in a '@'-postfix.
%
% Examples of 'Schema':
% + : * : (a*(b+c) = a*b+a*c) @ and_ops @ or_ops
% or identically (note the reversed order of generators)
% [+,*] : (a*(b+c) = a*b+a*c) @ [or_ops, and_ops]
%
% Usage:
% ?- [msschema].
% % Define to generators and_ops and or_ops.
% ?- assert(and_ops(and1(X,Y)/[X,Y])).
% ?- assert(and_ops(and2(X,Y)/[X,Y])).
% ?- assert(or_ops(or1(X,Y)/[X,Y])).
% ?- assert(or_ops(or2(X,Y)/[X,Y])).
% ?- instantiate([+,*] : (a*(b+c) = a*b+a*c) @ [or_ops, and_ops], Formula).
%
% More complex example, with a specific logic and the generator 'operators':
% ?- [multseq, msschema].
% ?- load_logic(luka).
% ?- instantiate([+,*] : (a*(b+c) = a*b+a*c) @ [operators(2), operators(2)], Formula, ActualReplacements).


instantiate(Schema, Formula) :-
    instantiate([], [], Schema, Formula, _ActualReplacements).

instantiate(Schema, Formula, ActualReplacements) :-
    instantiate([], [], Schema, Formula, ActualReplacements).

% instantiate(+Vars, +Replacements, +Schema, -Formula, -ActualReplacements)
instantiate(_, _, V, V, []) :-
    var(V),
    !.
instantiate(Vars, Replacements0, Schema@Generators0, Formula, ActualReplacements) :-
    !,
    (is_list(Generators0) ->
         Generators1 = Generators0
    ;    Generators1 = [Generators0]
    ),
    maplist(replace_shorthand, Generators1, Generators2),
    length(Generators2, N),
    length(Replacements1, N),
    maplist(call, Generators2, Replacements1),
    reverse(Replacements1, Replacements2),
    append(Replacements2, Replacements0, Replacements),
    instantiate(Vars, Replacements, Schema, Formula, ActualReplacements).
instantiate(Vars0, Replacements, NewVars0:Schema, Formula, ActualReplacements) :-
    !,
    (is_list(NewVars0) ->
         NewVars1 = NewVars0
    ;    NewVars1 = [NewVars0]
    ),
    reverse(NewVars1, NewVars2),
    append(NewVars2, Vars0, Vars1),
    instantiate(Vars1, Replacements, Schema, Formula, ActualReplacements).
instantiate(Vars, Replacements, Schema, Formula, ActualReplacements) :-
    Schema =.. [F|SchemaArgs],
    maplist(instantiate(Vars, Replacements), SchemaArgs, FormulaArgs, ActualReplacements0),
    replace(Vars, Replacements, F, FormulaArgs, Formula, FReplacement),
    flatten([FReplacement|ActualReplacements0], ActualReplacements1),
    sort(ActualReplacements1, ActualReplacements).

replace(Vars, Replacements, F, FormulaArgs, Formula, []) :-
    (Vars = []; Replacements = []),
    !,
    Formula =.. [F|FormulaArgs].
replace([F|_], [Replacement|_], F, FormulaArgs, Formula, [F:Replacement]) :-
    copy_term(Replacement, Formula/FormulaArgs),
    !.
replace([_|Vars], [_|Replacements], F, FormulaArgs, Formula, FReplacement) :-
    replace(Vars, Replacements, F, FormulaArgs, Formula, FReplacement).

operators(FArgs) :-
    operators(_, FArgs).
    
operators(N, Formula/Args) :-
    (var(N); integer(N)), !,
    setof(Functor/Arity,
          Op^A^B^C^(rule(Op^A,B,C), functor(Op, Functor, Arity)),
          Ops
         ),
    member(F/N, Ops),
    op_arity(F, N, Formula/Args).

op_arity(F, N, Formula/Args) :-
    length(Args, N),
    Formula =.. [F|Args].

replace_shorthand(F/A, op_arity(F, A)) :- !.
replace_shorthand(X, X).

% formulas(N, C, Ops, Fmla/Args) - generator for formulas/arguments pairs
% using operators in Ops, with N arguments, and complexity (number of
% operators used) C.

formulas(N, _, 0, X/Args) :-
  length(Args, N),
  member(X, Args).
formulas(N, Ops, 1, Op/Args) :-
  length(Args, N),
  member(Op, Ops),
  operator(Op, 0).
formulas(N, Ops, C, Fmla/Args) :-
  C>0,
  length(Args, N),
  member(Op, Ops),
  operator(Op, A),
  length(TArgs, A),
  Fmla =.. [Op|TArgs],
  maplist(mfunctor(Args), TArgs, FArgs),
  length(Cs, A),
  maplist(between(0,C), Cs),
  C1 is C-1,
  sum_list(Cs, C1),
  maplist(formulas(N, Ops), Cs, FArgs).

mfunctor(Args, X, X/Args).