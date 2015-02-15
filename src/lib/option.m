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

:- import_module mercury_mpm.formatting.

:- import_module char.
:- import_module getopt.

%----------------------------------------------------------------------------%

:- type option
    --->    help
    ;       debug
    ;       install_prefix
    ;       installed
    ;       version
    .

:- instance docable(option).
:- instance doc_ref(option).
:- instance doc_ref_values(option).

:- pred short_option(char::in, option::out) is semidet.

    % long_option(Name, Option):
    %
:- pred long_option(string::in, option::out) is semidet.

:- pred option_default(string::in, option::out, option_data::out) is multi.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.      % for `[|]'/2
:- import_module maybe.     % used for 'install_prefix' option
:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- instance docable(option) where [
    (func(to_doc/1) is option_to_doc)
].

:- instance doc_ref(option) where [
    (to_string(Option) = Name :- long_option_table(Name, Option))
].

:- instance doc_ref_values(option) where [
    (values(Option) :- long_option_table(_, Option))
].

short_option(h, help).
short_option(d, debug).
short_option(v, version).

long_option(Name, Option) :- long_option_table(Name, Option).

option_default(_Root, help, bool(no)).
option_default(_Root, debug, bool(no)).
option_default(Root, install_prefix, string(Root)).
option_default(_Root, installed, bool(no)).
option_default(_Root, version, bool(no)).

:- func option_to_doc(option) = doc.

option_to_doc(Option) = str(Help) :-
    option_help(Option, Help).

:- pred option_help(option, string).
:- mode option_help(in, out) is det.

option_help(help, "displays help about program usage").
option_help(debug, "enables debugging of package commands").
option_help(install_prefix, "specify where to install packages").
option_help(installed, "controls listing of local or installed packages").
option_help(version, "displays the versions of the library and executable").

:- pred long_option_table(string, option).
:- mode long_option_table(out, in) is det.
:- mode long_option_table(in, out) is semidet.
:- mode long_option_table(out, out) is multi.

long_option_table("help", help).
long_option_table("debug", debug).
long_option_table("install-prefix", install_prefix).
long_option_table("installed", installed).
long_option_table("version", version).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.option.
%----------------------------------------------------------------------------%
