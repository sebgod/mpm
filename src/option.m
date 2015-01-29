%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: option.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Thu 29 Jan 14:31:28 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Implements all options used throughout `libmercury_mpm'.
%----------------------------------------------------------------------------%

:- module mercury_mpm.option.

:- interface.

:- import_module mercury_mpm.documentation.

:- import_module char.
:- import_module getopt.

%----------------------------------------------------------------------------%

:- type option
    --->    help
    ;       debug
    ;       installed
    ;       version
    .

:- instance doc_ref(option).

:- pred short_option(char::in, option::out) is semidet.

    % long_option(Name, Option):
    %
:- pred long_option(string::in, option::out) is semidet.

:- pred option_default(option::out, option_data::out) is multi.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- instance doc_ref(option) where [
    (func(to_doc/1) is option_to_doc),
    (to_string(Option) = Name :- long_option_table(Name, Option)),
    (values(Option) :- long_option_table(_, Option))
].

short_option(h, help).
short_option(d, debug).
short_option(v, version).

long_option(Name, Option) :- long_option_table(Name, Option).

option_default(help, bool(no)).
option_default(debug, bool(no)).
option_default(installed, bool(no)).
option_default(version, bool(no)).

:- func option_to_doc(option) = doc.

option_to_doc(help) =
    str("displays help about program usage").
option_to_doc(debug) =
    str("enables debugging of package commands").
option_to_doc(installed) =
    str("controls listing of local or installed packages").
option_to_doc(version) =
    str("displays the versions of the library and executable.").

:- pred long_option_table(string, option).
:- mode long_option_table(out, in) is det.
:- mode long_option_table(in, out) is semidet.
:- mode long_option_table(out, out) is multi.

long_option_table("help", help).
long_option_table("debug", debug).
long_option_table("installed", installed).
long_option_table("version", version).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.option.
%----------------------------------------------------------------------------%
