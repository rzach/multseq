:- dynamic option/1.

is_option(strategy(leftright), def).
is_option(strategy(topdown), nodef).
is_option(strategy(ordering(_)), nodef).
is_option(strategy(interactive), nodef).
is_option(tex_rulenames(on), nodef).
is_option(tex_rulenames(off), def).
is_option(tex_sequents(signed), def).
is_option(tex_sequents(multidimensional), nodef).
is_option(tex_proofstyle(verbose), def).
is_option(tex_proofstyle(compact), nodef).
is_option(tex_proofstyle(bare), nodef).
is_option(proofs(on), def).
is_option(proofs(off), nodef).
is_option(tex_output(off), def).
is_option(tex_output(terse), nodef).
is_option(tex_output(verbose), nodef).
is_option(tex_success(on), def).
is_option(tex_success(off), nodef).
is_option(tex_failure(on), def).
is_option(tex_failure(off), nodef).

set_option([]) :-
    !.
set_option([O|Os]) :-
    !,
    set_option(O),
    set_option(Os).
set_option(O) :-
    is_option(O, _D),
    functor(O, F, A),
    functor(Q, F, A),
    retractall(option(Q)),
    assertz(option(O)).

reset_options :-
    is_option(O, def),
    set_option(O),
    fail.
reset_options.

list_options :-
    ms_writeln(['Active options:']),
    option(O),
    ms_writeln(['   ', O]),
    fail.
list_options :-
    ms_writeln,
    ms_writeln(['Available options:']),
    is_option(O, D),
    writel(['   ', O]),
    (D = def ->
       ms_writeln([' (default)'])
    ;  ms_writeln
    ),
    fail.
list_options :-
    ms_writeln.

:- reset_options.
