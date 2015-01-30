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

:- import_module mercury_mpm.documentation.

:- import_module bool.
:- import_module list.

%----------------------------------------------------------------------------%

:- instance doc_ref(cmd).

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

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- instance doc_ref(cmd) where [
    (func(to_doc/1) is cmd_to_doc),
    (func(to_string/1) is cmd_to_string),
    (values(Cmd) :- parse_cmd(Cmd, _, []))
].

parse_cmd(list)  --> ["list"].
parse_cmd(build) --> ["build"].

:- func cmd_to_string(cmd) = string.

cmd_to_string(Cmd) = String :-
    parse_cmd(Cmd, [String | _], []).

:- func cmd_to_doc(cmd) = doc.

cmd_to_doc(list)  = str("list dependencies of ?[packages]").
cmd_to_doc(build) = str("builds ?[local packages]").

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.command.
%----------------------------------------------------------------------------%
