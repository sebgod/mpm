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
%
% Special version constants used by the system:
%
%  * invalid package (cannot obtain package information)
%
% NOTE: these constants do violate the 'semver' standard on purpose.
% Code dealing with package information has to check for these constants.
%
% NOTE: for all special version strings versions `Major = -1' is true, and
% the name of the special constant is stored in the `Build' (last) part.
%

:- func invalid_package_version = version.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.documentation.

:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

version_to_string({Major, Minor, Patch, Pre, Build}) =
    ( if Major = -1 then
        Build
    else
        format("%d.%d.%d%s%s",
            [i(Major), i(Minor), i(Patch),
                s(Pre   = "" -> "" ; "-" ++ Pre),
                s(Build = "" -> "" ; "+" ++ Build)
            ])
    ).

version_to_doc(Version @ {Major, _Minor, _Patch, _Pre, Build}) =
    ( if Major = -1 then
        problem(Build)
    else
        str(version_to_string(Version))
    ).


%----------------------------------------------------------------------------%

invalid_package_version = {-1,0,0,"","<invalid package>"}.

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.semver.
%----------------------------------------------------------------------------%
