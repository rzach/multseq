% split(Es, Ls, E, Rs).
%    Selects an element E from list Es and
%    splits Es into the part Ls left of E and
%    the part Rs right of E.

split([E|Es], [], E, Es).
split([L|Es], [L|Ls], E, Rs) :-
	split(Es, Ls, E, Rs). 

% diff(As, Bs, Cs).     Cs contains all elements of As that are not a member of Bs.
diff([], _, []).
diff([A|As], Bs, Cs) :-
   ms_member(A, Bs), !,
   diff(As, Bs, Cs).
diff([A|As], Bs, [A|Cs]) :-
   diff(As, Bs, Cs).

% writel(Ts).
%    Writes Ts, a list of terms, succesively
%    to the current output file using
%    the built-in write/1.
%
% ms_writeln.
%    Writes end-of-line to current output.
%
% ms_writeln(Ts).
%    The same as writel/1, but adds a new-line
%    (nl/0) at the end.
%
% ms_writelns(List_of_Ts).
%    Writes each element of <List_of_Ts>, i.e., each
%    list of terms, using ms_writeln.

writel([]).
writel([A|As]) :-
	write(A),
	writel(As).

ms_writeln :-
	nl.

ms_writeln(As) :-
	writel(As),
	ms_writeln.

ms_writelns([]).
ms_writelns([L|Ls]) :-
	ms_writeln(L),
	ms_writelns(Ls).

% print_phrase(P).
%    Generate a string from DCG non-terminal <P>
%    and write it to current output.

print_phrase(P) :-
	phrase(P, S),
	put_string(S),
	!.

% print_tex(P).
%    Generate a string from DCG non-terminal <P>
%    and write it to tex_output.

print_tex(P) :-
	(option(tex_output(off)) ->
		true
	;	phrase(P, S),
		put_string(tex_output, S)),
	!.

% ensure that tex_output exists
:- 	set_stream(user_error, alias(tex_output)).

% put_string(S).
%    Write <S>, a list of ASCII codes, to standard output.

put_string([]).
put_string([C|S]) :-
	put(C),
	put_string(S).

put_string(_Stream, []).
put_string(Stream, [C|S]) :-
	put(Stream, C),
	put_string(Stream, S).

% phrase_list(Ps).
%    Generate successively the strings corresponding
%    to the DCG non-terminals in <Ps>.

phrase_list([]) --> [].
phrase_list([P|Ps]) -->
	P,
	phrase_list(Ps).

% cat(Filename).
%    Similar to the Unix command "cat".
%    Copies file <Filename> to the current output file.

cat(F) :-
	see(F),
	repeat,
	get0(Ch),
	( Ch < 0
	; put(Ch),
	  fail
        ),
	seen,
	!.

% sort_l(As, Bs).
%    Sort every element in <As> giving <Bs>.

sort_l([], []).
sort_l([A|As], [B|Bs]) :-
	sort(A, B),
	sort_l(As, Bs).

% nth(N, As, A)
%    <A> is the <N>th element of <As>. <N> has to be a number.

nth(1, [A|_], A).
nth(N0, [_|As], A) :-
	N0 > 1,
	N1 is N0 - 1,
	nth(N1, As, A).

% The following predicates are pre-defined in some Prologs,
% but not in others.
% The solution: we define them ourselves, using the prefix
% "ms" (which definitely has to do nothing with Microsoft).

ms_append([], Bs, Bs).
ms_append([A|As], Bs, [A|Cs]) :-
        ms_append(As, Bs, Cs).

ms_member(A, [A|_]).
ms_member(A, [_|As]) :-
        ms_member(A, As).

ms_reverse(As, Cs) :-
	ms_reverse(As, [], Cs).

ms_reverse([], Bs, Bs).
ms_reverse([A|As], Bs, Cs) :-
	ms_reverse(As, [A|Bs], Cs).

ms_select([E|Es], E, Es).
ms_select([E|Es], F, [E|Fs]) :-
	ms_select(Es, F, Fs).

ms_flatten([L], L).
ms_flatten([L|Ls], FL) :-
	ms_append(L, FL0, FL),
	ms_flatten(Ls, FL0).

% These predicates are assumed to be pre-defined in the Prolog system:
%    findall/3, name/2, sort/2, phrase/2, get0/1, nl/0, put/1, write/1

:- findall(S, ms_member(S, [1,2,3]), [1,2,3]).
:- name(abc, "abc").
:- name(123, "123").
:- name(012, "12").
:- phrase("abc", "abc").
:- sort([2,2,1,3], [1,2,3]).

% The axiom test relies on the fact that binary function symbols
% are sorted first by their first argument (propositional variable)
% and only then by the second argument (truth value).

:- sort([a^3,b^2,a^1,b^2], [a^1,a^3,b^2]).

% clear(S).             Retracts all clauses specified by S.

clear([]).
clear([S|Ss]) :-
   clear(S),
   clear(Ss).
clear(P/A) :-
   functor(H,P,A),
   retractall(H).
clear(P) :-
   atom(P),
   P \== [],
   G =..[P,S],
   G,
   clear(S).

