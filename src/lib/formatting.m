%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: formatting.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Thu 29 Jan 14:44:49 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Adds support for output formatting to `libmercury_mpm',
% especially for the command line interface.
%----------------------------------------------------------------------------%

:- module mercury_mpm.formatting.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- type doc_stream_nl_tuple == {doc, text_output_stream, bool}.

:- typeclass docable(T) where [
    func to_doc(T) = doc
].

:- typeclass doc_or_error(T) <= docable(T) where [
    pred is_error(T::in) is semidet
].

:- typeclass doc_ref(T) <= docable(T) where [
    func to_string(T) = string
].

:- typeclass doc_ref_values(T) <= doc_ref(T) where [
    pred values(T::out) is multi
].

:- instance docable(doc).
:- instance doc_or_error(doc).

:- instance docable(io.error).
:- instance doc_or_error(io.error).

:- pred is_stderr(doc_stream_nl_tuple::in) is semidet.

:- func make_docs(list(doc)) = doc_stream_nl_tuple.

:- func make_doc(T) = doc_stream_nl_tuple <= doc_or_error(T).

    % write_doc_opt_nl(Docable, !IO):
    %
    % writes `Docable' to 'stdout' or 'stderr' depending if 'is_error'/1 is
    % true, with a consecutive newline if needed.
    %
:- pred write_doc_opt_nl(T::in, io::di, io::uo) is det <= doc_or_error(T).

    % write_doc({Doc, Stream, Nl}, !IO):
    %
    % writes the `Doc' to `Stream' and adds a newline if `Nl' is 'yes'.
    %
:- pred write_doc(doc_stream_nl_tuple::in, io::di, io::uo) is det.

:- func error_to_doc(io.error) = doc.

:- type detail == bool.

:- func doc_ref_values = list(T) <= doc_ref_values(T).

:- func doc_ref_list_to_docs(detail, list(T)) = docs <= doc_ref(T).

    % Highlights a problem (red and bold)
    %
:- func problem(string) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- include_module mercury_mpm.formatting.ansi_escape_code.
:- import_module mercury_mpm.formatting.ansi_escape_code.

:- import_module solutions.
:- import_module string.        % for `++'/2
:- import_module std_util.      % for `id'/1

%----------------------------------------------------------------------------%

:- instance docable(doc) where [
    (func(to_doc/1) is std_util.id)
].

:- instance doc_or_error(doc) where [
    (is_error(_) :- false)
].

:- instance docable(io.error) where [
    (func(to_doc/1) is error_to_doc)
].

:- instance doc_or_error(io.error) where [
    (is_error(_) :- true)
].

is_stderr({_, stderr_stream, _}).

make_docs(Docs) = make_doc(docs(Docs)).

make_doc(Docable) =
    { to_doc(Docable)
    , (is_error(Docable) -> stderr_stream ; stdout_stream)
    , yes  % TODO: Optional newline detection
    }.

error_to_doc(Error) =
    group([problem("error: "), str(error_message(Error))]).

write_doc_opt_nl(Docable, !IO) :-
   write_doc(make_doc(Docable), !IO).

write_doc({Doc, Stream, Nl}, !IO) :-
    write_doc(Stream, Doc, !IO),
    (
        Nl = yes,
        nl(Stream, !IO)
    ;
        Nl = no
    ).

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

problem(String) = str(format_em(format_red(String))).

%----------------------------------------------------------------------------%
%
% Text property functions, currently implemented using ANSI escape codes,
% which are implemented in ansi.m

:- func format_em(string) = string.

format_em(String) = enclose_with_ansi_sgr_pair(String, bold).

:- func format_red(string) = string.

format_red(String) = enclose_with_ansi_sgr_pair(String, red).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.formatting.
%----------------------------------------------------------------------------%
