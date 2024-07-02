% msproperties.pl
% Specification of properties to check

% Each property is specified as
%
% property(PropName, OpList, Spec)
%
% where
%
% - PropName is the name of the property (an atom)
% - OpList is a list of operator variable/arity pairs
% - Spec is a specification
%
% Specifications are:
%
% - tautology(Fmla)
% - consequence([Prems] => Concl)
% - equivalence(Fmla1, Fmla2)
% - equality(Fmla1, Fmla2)
% - metaconseq([Hyps], Concl)
%
% Specifications are just Prolog queries, so can be negated using
% '\+' and combined using ',' and ';'.
%
% The formulas (and list of formulas Prems) must use the operators
% provided in OpList. (This is not enforced and may not be needed.)
%
% A query of the form chkProp(Omap, PropName) will determine if the
% property specified holds for the currently loaded logic, where the
% operators in the specification are replaced according to the mapping
% Omap. E.g., if the logic defines a 2-place operator imp, then
% chkProp([imp/(>)], impl1) will succeed if in that logic the formula
% a imp (b imp a) has a proof.
%
% To test if 'and' distributes (equationally) over 'or' and vice versa,
% ask both chkProp([and/(/\), or/(\/)], edistr) and
% chkProp([or/(/\), and/(\/)], edist).

% Wajsberg's axioms for Lukasiewicz logic
% Test using luka and
% chkProp([(&)/(/\),(v)/(\/),(=>)/(>)], wajsbergX).
% or lukasiewicz.msq and
% chkProp([and/(/\),or/(\/),imp/(>),(neg)/(-)], wajsbergX).

property(wajsberg1, [(>)/2], tautology(a > (b > a))).
property(wajsberg2, [(>)/2], tautology((a > b) > ((b > c) > (a > c)))).
property(wajsberg3, [(>)/2, (-)/1], tautology((((a > -a) > a) > a))).
property(wajsberg4, [(>)/2, (-)/1], tautology((-a > -b) > (b > a))).

% Bernays's axioms for classical logic
% Test using classical.msq and
% chkOmapProp([(neg)/(-),and/(/\),or/(\/),equiv/(=),imp/(>)],
% bernaysX)

property(bernays1, [(>)/2], tautology(a > (b > a))).
property(bernays2, [(>)/2], tautology((a > (a > b)) > (a > b))).
property(bernays3, [(>)/2], tautology((a > (b > c)) > (b > (a > c)))).
property(bernays4, [(>)/2], tautology((b > c) > ((a > b) > (a > c)))).

property(bernays5, [(>)/2, (/\)/2], tautology((a /\ b) > a)).
property(bernays6, [(>)/2, (/\)/2], tautology((a /\ b) > b)).
property(bernays7, [(>)/2, (/\)/2], tautology((a > b) > ((a > c) > (a > (b /\ c))))).

property(bernays8, [(>)/2, (\/)/2], tautology(a > (a \/ b))).
property(bernays9, [(>)/2, (\/)/2], tautology(b > (a \/ b))).
property(bernays10, [(>)/2, (\/)/2], tautology((b > a) > ((c > a) > ((b \/ c) > a)))).

property(bernays11, [(>)/2, (=)/2], tautology((a = b) > (a > b))).
property(bernays12, [(>)/2, (=)/2], tautology((a = b) > (b > a))).
property(bernays13, [(>)/2, (=)/2], tautology((a > b) > ((b > a) > (a = b)))).

property(bernays14, [(>)/2, (-)/1], tautology((a > b) > (-b > -a))).
property(bernays15, [(>)/2, (-)/1], tautology((a > (-a)) > (-a))).
property(bernays16, [(>)/2, (-)/1], tautology(a > -(-a))).
property(bernays17, [(>)/2, (-)/1], tautology(-(-(a)) > a)).

% Some famous intuitionistically invalid classical tautologies

% Law of excluded middle
property(lem, [(\/)/2, (-)/1], tautology(a \/ (-a))).

% Weak LEM
property(weaklem, [(\/)/2, (-)/1], tautology((-a) \/ (-(-a)))).

property(prelinearity, [(\/)/2, (>)/2], tautology((a > b) \/ (b > a))).

% Peirce's Law
property(peirce, [(>)/2], tautology((((a > b) > a) > a))).

% Consequentia mirabilis
property(mirabilis, [(>)/2, (-)/1], tautology(((-a) > a) > a)).

% Kreisel-Putnam
property(kp, [(\/)/2, (>)/2, (-)/1], tautology(((-a) > (b \/ c)) > (((-a) > b) \/ ((-a) > c)))).

% More classical tautologies that fail in some popular logics

% Mingle
property(mingle, [(>)/2], tautology(a > (a > a))).

% Pseudo modus ponens
property(pseudomp, [(>)/2, (/\)/2], tautology((a /\ (a > b)) > b)).

% Prefixing
property(prefix, [(>)/2], tautology((a > b) > ((c > a) > (c > b)))).

% Suffixing
property(suffix, [(>)/2], tautology((a > b) > ((b > c) > (a > c)))).

% Contraction
property(contraction, [(>)/2], tautology((a > (a > b)) > (a > b))).

% Reductio
property(reductio, [(>)/2, (-)/1], tautology((a > (-a)) > (-a))).


% consequence(Premises => Conclusion) -- test if logical consequence holds

% some standard consequences

property(modusponens, [(>)/2],
  consequence([a, a > b] => b)).         % a, a > b |= b

property(modustollens, [(>)/2, (-)/1],
  consequence([a > b, -b] => -a)).       % a > b, -b |= -a

property(hyposyllogism, [(>)/2],
  consequence([a > b, b > c] => a > c)).

property(disjsyllogism, [(\/)/2, (-)/1],
  consequence([a \/ b, -a] => b)).

property(destrdilemma, [(\/)/2, (>)/2, (-)/1],
  consequence([(-c) \/ (-d), a > c, b > d] => ((-a) \/ (-b)))).

property(constrdilemma, [(\/)/2, (>)/2],
  consequence([a \/ b, a > c, b > d] => (c \/ d))).

property(importation, [(/\)/2, (>)/2],
  consequence([a > (b > c)] => ((a /\ b) > c))).

property(exportation, [(/\)/2, (>)/2],
  consequence([(a /\ b) > c] => a > (b > c))).

% Lewis's "fallacies"

% hyposyllogism above

% contraposition
property(contrapos1, [(-)/1, (>)/2], consequence([a > b] => (-b) > (-a))).
property(contrapos2, [(-)/1, (>)/2], consequence([(-a) > (-b)] => b > a)).

% Agglomeration (conditional and-introduction)
property(agglomeration, [(/\)/2, (>)/2], consequence([a > b, a > c] => (a > (b /\ c)))).

% simplification of disjunctive antecedents
property(sda, [(\/)/2, (/\)/2, (>)/2], consequence([(a \/ b) > c] => ((a > c) /\ (b > c)))).

% equivalence(Frml1, Frmla2) -- test if a logical equivalence
% (bi-implication) holds

% logical distributivity -- require left /\ right distributivity, each
% is a logical biimplication

property(ldistrleft, [(/\)/2,(\/)/2],
  equivalence( a /\ (b \/ c), (a /\ b) \/ (a /\ c) )).

property(ldistrright, [(/\)/2,(\/)/2],
  equivalence( (b \/ c) /\ a, (b /\ a) \/ (c /\ a) )).

% Font's proof system for FDE consist of rules only (no tautologies)
% Test using fde.msq and 
% chkOmapProp([(neg)/(-),and/(/\),or/(\/)], fontX).

property(font1, [(/\)/2], consequence([a /\ b] => a)).
property(font2, [(/\)/2], consequence([a /\ b] => b)).
property(font3, [(/\)/2], consequence([a, b] => a /\ b)).

property(font4, [(\/)/2], consequence([a] => a \/ b)).
property(font5, [(\/)/2], consequence([a \/ b] => b \/ a)).
property(font6, [(\/)/2], consequence([a \/ a] => a)).
property(font7, [(\/)/2], consequence([a \/ (b \/ c)] => (a \/ b) \/ c)).

property(font8, [(/\)/2, (\/)/2], consequence([a \/ (b /\ c)] => (a \/ b) /\ (a \/ c))).
property(font9, [(/\)/2, (\/)/2], consequence([(a \/ b) /\ (a \/ c)] => a \/ (b /\ c))).

property(font10, [(\/)/2], consequence([a \/ c] => (-(-a)) \/ c)).
property(font11, [(\/)/2], consequence([-(-a) \/ c] => a \/ c)).
property(font12, [(/\)/2,(\/)/2], consequence([-(a \/ b) \/ c] => (-a /\ -b) \/ c)).
property(font13, [(/\)/2,(\/)/2], consequence([(-a /\ -b) \/ c] => -(a \/ b) \/ c)).
property(font14, [(/\)/2,(\/)/2], consequence([-(a /\ b) \/ c] => (-a \/ -b) \/ c)).
property(font15, [(/\)/2,(\/)/2], consequence([(-a \/ -b) \/ c] => -(a /\ b) \/ c)).

% equality(Fmla1, Fmla2) -- test if two formulas are algebraically equal

% eidiem -- equational idempotence

property(eidem, [(/\)/2], equality(a, a /\ a)).

% edistr -- equational distributivity (both left and right).
% Note that the specification is a conjunction here!

property(edistr, [(/\)/2,(\/)/2],
  ( equality(a /\ (b \/ c), (a /\ b) \/ (a /\ c)),
    equality((b \/ c) /\ a, (b /\ a) \/ (c /\ a)))).

% definabilities

property(defimpor, [(>)/2, (-)/1, (\/)/2], equality(a > b, (-a) \/ b)).
property(defimpand, [(>)/2, (-)/1, (/\)/2], equality(a > b, -(a /\ (-b)))).
property(deforimp, [(>)/2, (\/)/2], equality(a \/ b, (a > b) > b)).
property(deforand, [(/\)/2, (\/)/2, (-)/1], equality(a \/ b, -((-a) /\ (-b)))).
property(defandimp, [(>)/2, (/\)/2, (-)/1], equality(a /\ b, -(a > (-b)))).
property(defandor, [(/\)/2, (-)/1, (\/)/2], equality(a /\ b, -((-a) \/ (-b)))).

% De Morgan Laws

property(demorganor, [(/\)/2, (\/)/2, (-)/1], equality(-(a \/ b), (-a) /\ (-b))).
property(demorganand, [(/\)/2, (\/)/2, (-)/1], equality(-(a /\ b), (-a) \/ (-b))).

% metaconsequences -- deduction theorem, residuation

property(deductionthm, [(>)/2], metaconseq([[p, q] => r], [p] => (q > r) )).
property(residuation, [(>)/2, (/\)/2], metaconseq([[p /\ q] => r], [p] => (q > r) )).
