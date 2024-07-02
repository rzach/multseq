% Test file to find definitions of operators operators

% make sure MUltseq is loaded
:- ensure_loaded('../multseq/multseq').

% load the rules
:- load_logic('lukasiewicz.msq').

% define standard Omap
:- setOmap([(neg)/(-),imp/(>),and/(/\),or/(\/),equiv/(=)]). 

% check all properties and write report to out.tex

:- set_option(tex_output(terse)).
:- set_option(tex_failure(off)).

:- start_logging(ex_lukasiewicz3,'.tex').

:- print_tex(tex_title("Expressing Connectives in \\L ukasiewicz logic")).

:- print_tex(tex_paragraph(["In this example we search for formulas that define any 2-place connective. We search for defining formulas containing up to 3 connectives. Negation is not definable."])).

:- print_tex(tex_logic).

:- bagof(X, A^operator(X, A), Ops),
  ( operator(Op,2),
    print_tex(tex_section(["Equivalents of $", tex_conn(Op), "$"])),
    between(0, 3, N),
    subtract(Ops, [Op], ToRep),
    instantiate(+ : a+b @ formulas(2, ToRep, N), X),
    F =.. [Op, a, b],
    equality(F, X),
    fail)
  ; true.

:- print_tex(tex_listing("ex_lukasiewicz3.pl")).

:- stop_logging.