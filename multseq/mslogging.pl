% Logging an interactive session to a file
%
% Interactive commands as well as the answers by the interpreter are
% logged to the file with a preceding percent sign (TeX/Prolog comment),
% while everything written to the stream 'session_log' will be written
% verbatim.
%
% How to use:
%
% If needed, define the predicates logging_intro/0 and logging_outro/0.
% They will be executed immediately after starting and before ending
% logging, respectively.
%
% Logging is started by start_logging/0, start_logging/1 or start_logging/2.
% Each of the predicates defines a stream 'session_log' that can be used to
% write to the log, like write(session_log, 'Hello!').
% The commands differ in the way the log file is named.
% start_logging: The log file is named session_DATETIME.txt, like
%    session_2024-06-28T17:08:45+02:00.txt
% start_logging(Extension): like start_logging/0, but using Extension
%    instead of '.txt'
% start_logging(LogFileName, Extension): The name of the log file is
%    formed by concatenating the two arguments.
%
% Logging is stopped by executing stop_logging/0 or by stopping the
% interpreter by halt, end_of_file or ^D.

:- dynamic logging_intro/0, logging_outro/0, default_LogfileName/1, default_Extension/1.
:- multifile user:message_hook/3, logging_intro/0, logging_outro/0.

default_LogfileName(LogFileName) :-
    get_time(T),
    format_time(atom(A), '%FT%T%:z', T, posix),
    atom_concat('session_', A, LogFileName).

default_Extension('.tex').

start_logging :-
    default_LogfileName(LogFileName),
    default_Extension(Extension),
    start_logging(LogFileName, Extension).

start_logging(Extension) :-
    default_LogfileName(LogFileName),
    start_logging(LogFileName, Extension).

start_logging(LogFileName, Extension) :-
    atom_concat(LogFileName, Extension, LogFileNameExtension),
    open(LogFileNameExtension, write, _, [alias(session_log)]),
    assert((user:expand_query(Query,Query,Bindings,Bindings) :-
                logQuery(Query,Bindings))),
    assert((prolog:expand_answer(Goal,Bindings,Bindings) :-
                user:logAnswer(Goal,Bindings))),
    assert((user:message_hook(query(no), Kind, Lines) :-
                logFail(Kind, Lines), fail)),
    ignore(logging_intro).

logQuery(Query,Bindings) :-
    % if interpreter is stopped, execute stop_logging code
    % but do not log stop command
    member(Query, [halt, end_of_file]) ->
        stop_logging
    % do not log stop_logging command
    ; Query == stop_logging ->
        true
    ;
        copy_term([Query, Bindings], [Q,B]),
        maplist(bind,B),
        % b_setval(last_query, Q),
        write_term(session_log, '', [nl]),
        write_term(session_log, '% ', []),
        write_term(session_log, Q, []),
        write_term(session_log, '', [fullstop, nl]).

bind(X=X).

logAnswer(Goal,Bindings) :-
    (Goal = _:G ->
         true
    ; Goal = G
    ),
    (G == user:start_logging ->
        true
    ;
        maplist(write_binding, Bindings)%,
%        (current_predicate(texify/2),
%         texify(G, G_TeX) ->
%            write_term(session_log, G_TeX, [nl])
%        ;
%            true
%        )
    ).

logFail(_Kind, _Lines) :-
    % b_getval(last_query, Q),
    write_term(session_log, '% false', [nl]).

write_binding(B) :-
    write_term(session_log, '%   ', []),
    write_term(session_log, B, []),
    write_term(session_log, '', [nl]).

stop_logging :-
    ignore(logging_outro),
	close(session_log, []),
    retract((expand_query(Query,Query,Bindings,Bindings) :-
                 logQuery(Query,Bindings))),
    retract((prolog:expand_answer(Goal,Bindings,Bindings) :-
                 user:logAnswer(Goal,Bindings))),
    retract((user:message_hook(query(no), Kind, Lines) :-
                 logFail(Kind, Lines), fail)).