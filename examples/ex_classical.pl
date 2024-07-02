% Test file to check things in classical logic

% make sure MUltseq is loaded
:- ensure_loaded('../multseq/multseq').

% load sample properties
:- [properties].

% load the rules
:- load_logic('classical.msq').

% define standard Omap
:- setOmap([(neg)/(-),imp/(>),and/(/\),or/(\/),equiv/(=)]). 

% check all properties and write report to out.tex

:- set_option(tex_output(verbose)).
:- set_option(tex_sequents(multidimensional)).
:- set_option(tex_rulenames(on)).

:- start_logging(ex_classical,'.tex').

:- print_tex(tex_title("Report on Classical Logic")).

:- print_tex(tex_logic).

:- print_tex(tex_paragraph(["We verify that all classical logic satisfies
      some well-known properties (involving only $\\land$, $\\lor$,
      $\\to$, $\\neg$, and $\\leftrightarrow$). We output proofs in
      ``multidimensional'' format, which for classical logic means just
      two sides to a sequent,
      as usual."])).

:- print_tex(tex_section(["Bernays's axioms for classical logic"])).

:- (member(X,[bernays1,bernays2,bernays3,bernays4,bernays5,bernays6,bernays7,bernays8,bernays9,bernays10,bernays11,bernays12,bernays13,bernays14,bernays15,bernays16,bernays17]), chkProp(X), fail; true).

:- print_tex(tex_section(["Classical tautologies not intuitionistically valid"])).

:- (member(X,[lem,weaklem,prelinearity,peirce]), chkProp(X), fail; true).

:- print_tex(tex_section(["Some popular consequences"])).

:- (member(X,[modusponens,modustollens,hyposyllogism,disjsyllogism,destrdilemma,constrdilemma,importation,exportation]), chkProp(X), fail; true).

:- print_tex(tex_section(["Some popular equivalences"])).

:- (member(X,[ldistrright,ldistrleft]), chkProp(X), fail; true).
% Here we switch and and or
:- (member(X,[ldistrright,ldistrleft]), chkProp([or/(/\),and/(\/)],X), fail; true).

:- print_tex(tex_section(["Interdefinability of connectives"])).

:- (member(X,[defimpor,defimpand,deforimp,deforand,defandimp,deforand]), chkProp(X), fail; true).

:- print_tex(tex_section(["Metaconsequences"])).

:- (member(X,[deductionthm,residuation]), chkProp(X), fail; true).

:- print_tex(tex_listing("ex_classical.pl")).

:- stop_logging.