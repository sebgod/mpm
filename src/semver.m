%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: semver.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Jan 21 13:54:35 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% This module implements the http://semver.org standard (2.0.0).
%----------------------------------------------------------------------------%

:- module mercury_mpm.semver.

:- interface.

:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- type version == {int, int, int, string, string}.

:- func version_to_string(version) = string.

:- func version_to_doc(version) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

version_to_string({Major, Minor, Patch, Pre, Build}) =
    format("%d.%d.%d%s%s",
        [i(Major), i(Minor), i(Patch),
            s(Pre   = "" -> "" ; "-" ++ Pre),
            s(Build = "" -> "" ; "+" ++ Build)
        ]).

version_to_doc(Version) = str(version_to_string(Version)).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.semver.
%----------------------------------------------------------------------------%
