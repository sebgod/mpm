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
    --->    available
    ;       help
    ;       debug
    ;       install_prefix
    ;       installed
    ;       version
    .

:- instance docable(option).
:- instance doc_ref(option).
:- instance doc_ref_values(option).

    % short_option(ShortChar, Option):
    %
    % Is true for all `Option' values for which a `ShortChar' can be used.
    %
:- pred short_option(char::in, option::out) is semidet.

    % long_option(Name, Option):
    %
    % `Option' is the typed 'option' iff `Name' is a valid command line
    % option.
    %
:- pred long_option(string::in, option::out) is semidet.

    % option_default(Option, OptionData):
    %
    % Outputs the associated `OptionData' for each `Option'.
    %
:- pred option_default(option::out, option_data::out) is multi.

%----------------------------------------------------------------------------%
%
% General helper functions for option parsing.
%

    % lookup_maybe_string_or_default_option(OptionTable, Option, DefaultValue,
    %   Value):
    %
    % If the `Option' value is 'yes(Value0)', then `Value' unifies with
    % `Value0', else with `DefaultValue'.
    %
:- pred lookup_maybe_string_or_default_option(option_table(Option)::in,
    Option::in, string::in, string::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.  % for `[|]'/2
:- import_module maybe. % used for `lookup_maybe_string_or_default_option'/4
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

option_default(available, bool(no)).
option_default(help, bool(no)).
option_default(debug, bool(no)).
option_default(install_prefix, maybe_string(no)).
option_default(installed, bool(no)).
option_default(version, bool(no)).

:- func option_to_doc(option) = doc.

option_to_doc(Option) = str(Help) :-
    option_help(Option, Help).

:- pred option_help(option, string).
:- mode option_help(in, out) is det.

option_help(available, "list will operate on all available packages").
option_help(help, "displays help about program usage").
option_help(debug, "enables debugging of package commands").
option_help(install_prefix, "specify where to install packages").
option_help(installed, "controls listing of local or installed packages").
option_help(version, "displays the versions of the library and executable").

:- pred long_option_table(string, option).
:- mode long_option_table(out, in) is det.
:- mode long_option_table(in, out) is semidet.
:- mode long_option_table(out, out) is multi.

long_option_table("available", available).
long_option_table("help", help).
long_option_table("debug", debug).
long_option_table("install-prefix", install_prefix).
long_option_table("installed", installed).
long_option_table("version", version).

%----------------------------------------------------------------------------%

lookup_maybe_string_or_default_option(OptionTable, Option, DefaultValue,
    Value) :-
    lookup_maybe_string_option(OptionTable, Option, MaybeValue),
    (
        MaybeValue = yes(Value0),
        Value = Value0
    ;
        MaybeValue = no,
        Value = DefaultValue
    ).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.option.
%----------------------------------------------------------------------------%
