
---
title: MUltseq $ms_version$
---

MUltseq is a program that can be used to decide the validity of
finitely-valued formulas, the consequence relation, and the validity
of equations and quasi-equations in certain finite algebras. In its
core, MUltseq is a generic sequent prover for propositional
finitely-valued logics. The sequent systems used are the ones
described by @BaazFermullerZach1994, @BaazFermullerSalzerZach1998, and
@Zach1993.

Multseq is intended as companion for
[MUltlog](https://www.logic.at/multlog/), which computes optimized
sequent rules from the truth tables of a finitely-valued logic.
MUltseq uses these rules to construct derivations—automatically or
interactively—for sequents given directly by the user or generated
by the system; the results can be typeset using LaTeX.

A short description of MUltseq was presented at the "Joint conference
of the 5th Barcelona Logic Meeting and the 6th Kurt Gödel Colloquium"
in June 1999 @GilSalzer1999. A "marriage" of MUltlog and MUltseq was
described in @BFGPS03. Old versions of MUltseq are still available as [gzipped
tar archives](http://www.logic.at/multseq/distr/).

Version 2.0 of Multlog introduces the ability to pre-define properties
of a logic and to use the MUltseq prover to test them in a given
logic. Properties can be tautologies (e.g., law of excluded middle),
consequences (e.g., modus ponens), equivalences (e.g., De Morgan's
laws), quasi-equations (e.g., stating the inter-definability of operators),
and meta-consequences (e.g., the deduction theorem).

## Installation

MUltseq can be obtained from
[github.com/rzach/multseq](https://github.com/rzach/multseq).

For installation instructions, see the [MUltseq user
manual](multseq.html) (also available [in PDF](multseq.pdf)).

If you encounter problems, please [file an
issue](https://github.com/rzach/multseq/issues).

## Examples

A number of examples are provided in the
[examples](https://github.com/rzach/multseq/tree/main/examples)
directory. They result in the following PDFs:

$for(example)$
  - [$example.name$]($example.link$)
$endfor$

## About MUltseq

MUltseq was originally developed by [Àngel
Gil](https://www.esci.upf.edu/en/lecturer/dr-angel-gil-717)
(Barcelona) and [Gernot Salzer](http://www.logic.at/staff/salzer)
(Vienna) within the framework of an Acción integrada titled "Generic
Decision Procedures for Many-valued Logics". Version 2.0 is joint work
with [Richard Zach](https://richardzach.org) (Calgary)

## References