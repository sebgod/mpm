%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: command.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Sun Jan 25 12:41:06 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% A command is the core of the API, representing the action the user wants to
% perform on packages.
%----------------------------------------------------------------------------%

:- module mercury_mpm.command.

:- interface.

:- import_module list.
:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- type cmd_refs == list(cmd_ref).

:- type cmd_ref == { cmd, string }.

    % for a documentation of these types,
    % please refer to `cmd_to_docs'/1 in this module.
    %
:- type cmd
    --->    list
    ;       build.

:- pred parse_cmd(cmd, list(string), list(string)).
:- mode parse_cmd(out, in, out) is semidet.
:- mode parse_cmd(in, out(non_empty_list), in) is det.
:- mode parse_cmd(out, out(non_empty_list), in) is multi.

:- func to_string(cmd) = string.

:- func to_doc(cmd) = doc.

:- func cmd_to_docs(cmd) = docs.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

parse_cmd(list)  --> ["list"].
parse_cmd(build) --> ["build"].

to_string(Cmd) = String :-
    parse_cmd(Cmd, [String | _], []).

to_doc(Cmd) = docs(cmd_to_docs(Cmd)).

cmd_to_docs(list)  = [str("list dependencies of ?[packages")].
cmd_to_docs(build) = [str("builds ?[local packages]")].

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.command.
%----------------------------------------------------------------------------%
