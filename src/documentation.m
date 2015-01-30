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
    func to_string(T) = string,
    (pred values(T::out) is multi)
].

:- type detail == bool.

:- func doc_ref_values = list(T) <= doc_ref(T).

:- func doc_ref_list_to_docs(detail, list(T)) = docs <= doc_ref(T).

:- func error_to_doc(io.error) = doc.

:- func error_message_to_doc(string) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.

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
    group([str("\x1b\[31;1merror: \x1b\[0m"), str(Message)]).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.documentation.
%----------------------------------------------------------------------------%
