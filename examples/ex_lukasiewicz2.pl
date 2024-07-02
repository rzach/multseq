% Test file to compare strong and weak Lukasiewicz operators

% make sure MUltseq is loaded
:- ensure_loaded('../multseq/multseq').

% load sample properties
:- [properties].

% load the rules
:- load_logic('lukasiewicz.msq').

% define standard Omap
:- setOmap([(neg)/(-),imp/(>),and/(/\),or/(\/),equiv/(=)]). 

% check all properties and write report to out.tex

:- set_option(tex_output(terse)).

:- start_logging(ex_lukasiewicz2,'.tex').

:- print_tex(tex_title("Comparing Strong and Weak \\L ukasiewicz Logic Connectives")).

:- print_tex(tex_logic).

:- (compareProp([[and,sand]/(/\),[or,sor]/(\/)], _), fail); true.

:- print_tex(tex_listing("ex_lukasiewicz2.pl")).

:- stop_logging.