%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: documentation.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Thu 29 Jan 14:44:49 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Adds support for documentation functions to `libmercury_mpm',
% espacially for the command line interface.
%----------------------------------------------------------------------------%

:- module mercury_mpm.documentation.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- typeclass doc_ref(T) where [
    func to_doc(T) = doc,
    func to_string(T) = string
].

:- typeclass doc_ref_values(T) <= doc_ref(T) where [
    (pred values(T::out) is multi)
].

:- type detail == bool.

:- func doc_ref_values = list(T) <= doc_ref_values(T).

:- func doc_ref_list_to_docs(detail, list(T)) = docs <= doc_ref(T).

:- func error_to_doc(io.error) = doc.

:- func error_message_to_doc(string) = doc.

    % Highlights a problem (currently using ANSI colours)
    %
:- func problem(string) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.
:- import_module string.        % for `++'/2

%----------------------------------------------------------------------------%

doc_ref_values = solutions(values).

doc_ref_list_to_docs(Detailed, DocRefs) = Docs :-
    (
        Detailed = yes,
        FmtFunc =
            (func(DocRef) =
                group([
                    str(to_string(DocRef)), str(" - "),
                    indent([to_doc(DocRef)]),
                    hard_nl
                ])
            ),
        OptNl = []
    ;
        Detailed = no,
        FmtFunc = (func(DocRef) = docs([str(to_string(DocRef)), str(" ")])),
        OptNl = [nl]
    ),
    Docs = map(FmtFunc, DocRefs) ++ OptNl.

error_to_doc(Error) = error_message_to_doc(error_message(Error)).

error_message_to_doc(Message) =
    group([problem("merror: "), str(Message)]).

problem(String) = str("\x1b\[31;1m" ++ String ++ "\x1b\[0m").

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.documentation.
%----------------------------------------------------------------------------%
