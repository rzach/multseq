% Test file to check things in Lukasiewicz logic

% make sure MUltseq is loaded
:- ensure_loaded('../multseq/multseq').

% load sample properties
:- [properties].

% load the rules
:- load_logic('lukasiewicz.msq').

% define standard Omap
:- setOmap([(neg)/(-),imp/(>),and/(/\),or/(\/),equiv/(=)]). 

% check all properties and write report to out.tex

:- set_option(tex_output(verbose)).

:- start_logging(ex_lukasiewicz1,'.tex').

:- print_tex(tex_title("Report on 3-Valued \\L ukasiewicz Logic")).

:- print_tex(tex_paragraph(["We check a number of properties in the 3-valued \\L ukasiewicz logic."])).

:- print_tex(tex_logic).

:- print_tex(tex_section(["Wajsberg's axioms for \\L ukasiewicz logic"])).

:- (member(X,[wajsberg1,wajsberg2,wajsberg3,wajsberg4]), chkProp(X), fail; true).

:- print_tex(tex_section(["Bernays's axioms for classical logic"])).

% leaving out bernays11-13 as these involve equvalence

:- set_option(tex_output(terse)).

:- (member(X,[bernays1,bernays2,bernays3,bernays4,bernays5,bernays6,bernays7,bernays8,bernays9,bernays10,bernays14,bernays15,bernays16,bernays17]), chkProp(X), fail; true).

:- set_option(tex_output(verbose)).

:- print_tex(tex_section(["Classical tautologies not intuitionistically valid"])).

:- (member(X,[lem,weaklem,bernays15,prelinearity,mirabilis,peirce]), chkProp(X), fail; true).

:- print_tex(tex_section(["Some more interesting tautologies"])).

:- (member(X,[mingle,pseudomp,prefix,suffix,contraction,reductio]), chkProp(X), fail; true).


:- print_tex(tex_section(["Some popular consequences"])).

:- (member(X,[modusponens,modustollens,hyposyllogism,disjsyllogism,destrdilemma,constrdilemma,importation,exportation,contrapos1,contrapos2,agglomeration,sda]), chkProp(X), fail; true).


:- print_tex(tex_section(["Some popular equivalences"])).

:- (member(X,[ldistrright,ldistrleft]), chkProp(X), fail; true).
% Here we switch and and or
:- (member(X,[ldistrright,ldistrleft]), chkProp([or/(/\),and/(\/)],X), fail; true).


:- print_tex(tex_section(["Interdefinability of connectives"])).

:- (member(X,[defimpor,defimpand,deforimp,deforand,defandimp,deforand]), chkProp(X), fail; true).


:- print_tex(tex_section(["Metaconsequences"])).

:- (member(X,[deductionthm,residuation]), chkProp(X), fail; true).

:- print_tex(tex_listing("ex_lukasiewicz1.pl")).

:- stop_logging.