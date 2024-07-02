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