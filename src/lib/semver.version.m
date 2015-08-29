%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: semver.version.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Apr  8 12:30:43 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% This module implements the http://semver.org standard (2.0.0).
%----------------------------------------------------------------------------%

:- module mercury_mpm.semver.version.

:- interface.

:- import_module mercury_mpm.dcg_parsing.

:- import_module pretty_printer. % for the doc type

%----------------------------------------------------------------------------%

:- type dist_version == {int, int, int, string}.
:- type version == {dist_version, string}.

:- func version_to_string(version) = string.

:- func det_string_to_version(string) = version.

:- pred string_to_version(string, version).
:- mode string_to_version(di, uo) is semidet.
:- mode string_to_version(in, out) is semidet.

:- func version_to_doc(version) = doc.

:- pred parse_version : parser1_pred(version).
:- mode parse_version(uo, di, muo) is semidet.
:- mode parse_version(uo, mdi, muo) is semidet.
:- mode parse_version(uo, in, out) is semidet.

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

:- func any_version = (version::uo) is det.

:- func invalid_package_version = (version::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.formatting.

:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

version_to_string({{Major, Minor, Patch, Pre}, Build}) =
    ( if Major = -1 then
        Build
    else
        format("%d.%d.%d%s%s",
            [i(Major), i(Minor), i(Patch),
                s(Pre   = "" -> "" ; "-" ++ Pre),
                s(Build = "" -> "" ; "+" ++ Build)
            ])
    ).

det_string_to_version(VersionString) =
    ( if
        string_to_version(VersionString, Version)
    then
        Version
    else
        invalid_package_version
    ).

string_to_version(VersionString, Version) :-
    ( if
        VersionString = version_to_string(invalid_package_version)
    then
        Version = invalid_package_version
    else
        Chars = to_char_list(VersionString),
        parse_version(Version, Chars, [])
    ).

parse_version({{Major, Minor, Patch, Pre}, Build}) -->
    dec_unsigned_int(Major), ['.'],
    dec_unsigned_int(Minor), ['.'],
    dec_unsigned_int(Patch),
    % TODO: Parse pre + build
    { Pre = "",
      Build = ""  }.

version_to_doc(Version @ {{Major, _Minor, _Patch, _Pre}, Build}) =
    ( if Major = -1 then
        problem(Build)
    else
        str(version_to_string(Version))
    ).

%----------------------------------------------------------------------------%

any_version = {{-1,0,0,""},"*"}.
invalid_package_version = {{-1,0,0,""},"<invalid package>"}.

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.semver.version.
%----------------------------------------------------------------------------%
