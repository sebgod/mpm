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

:- instance docable(command).
:- instance doc_ref(command).
:- instance doc_ref_values(command).

    % for a documentation of these types,
    % please refer to `command_to_docs'/1 in this module.
    %
:- type command
    --->    list
    ;       build.

:- pred parse_command(command, list(string), list(string)).
:- mode parse_command(out, in, out) is semidet.
:- mode parse_command(in, out(non_empty_list), in) is det.
:- mode parse_command(out, out(non_empty_list), in) is multi.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- instance docable(command) where [
    (func(to_doc/1) is command_to_doc)
].

:- instance doc_ref(command) where [
    (func(to_string/1) is command_to_string)
].

:- instance doc_ref_values(command) where [
    (values(Command) :- parse_command(Command, _, []))
].

parse_command(list)  --> ["list"].
parse_command(build) --> ["build"].

:- func command_to_string(command) = string.

command_to_string(Command) = String :-
    parse_command(Command, [String | _], []).

:- func command_to_doc(command) = doc.

command_to_doc(list)  = str("list dependencies of ?[packages]").
command_to_doc(build) = str("builds ?[local packages]").

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.command.
%----------------------------------------------------------------------------%
