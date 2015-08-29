%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: semver.range.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Apr  8 12:37:23 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Test semver ranges
%----------------------------------------------------------------------------%

:- module mercury_mpm.semver.range.

:- interface.

:- import_module mercury_mpm.semver.
:- import_module mercury_mpm.semver.version.

:- import_module pretty_printer. % for the doc type

%----------------------------------------------------------------------------%

    % A version range, e.g. 1.2.* or 1.2.10-1.3.*
    %
:- type range
    --->    v(version)
    ;       r(version, version)
    ;       w(wildcard_version).

:- type wildcard_version
    --->    wildcard_version(
                wv_version  ::  version,
                wv_wildcard ::  wildcard
            ).

:- type wildcard
    --->    any
    ;       minor
    ;       patch.

    % det_string_to_range(RangeString) = Range:
    %
    % Builds `Range' by parsing the `RangeString' or throws an exception
    % iff `RangeString' is not a valid range.
    %
:- func det_string_to_range(string) = range.

:- pred string_to_range(string, range).
:- mode string_to_range(di, uo) is semidet.
:- mode string_to_range(in, out) is semidet.

:- pred parse_range : parser1_pred(range).
:- mode parse_range(uo, di, muo) is semidet.
:- mode parse_range(uo, mdi, muo) is semidet.
:- mode parse_range(uo, in, out) is semidet.

:- func range_to_doc(range) = doc.

%----------------------------------------------------------------------------%
%
% Special version constants used by the system,
% see semver.version.m for details
%

:- func invalid_range = (range::uo) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.dcg_parsing.
:- import_module mercury_mpm.formatting.

:- import_module char.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

det_string_to_range(RangeString) =
    ( if
        string_to_range(RangeString, Range)
    then
        Range
    else
        invalid_range
    ).

string_to_range(RangeString, Range) :-
    Chars = to_char_list(RangeString),
    parse_range(Range, Chars, []).

parse_range(Range) -->
    ( if
        parse_version(Lo),
        ws_opt
    then
        ( if [-], ws_opt then
            parse_version(Hi), ws_opt,
            { Range = r(Lo, Hi) }
        else
            { Range = v(Lo) }
        )
    else if
        [*],
        ws_opt
    then
        { Range = w(wildcard_version(any_version, any)) }
    else if
        dec_unsigned_int(Major),
        ['.', '*'],
        ws_opt
    then
        { Range = w(wildcard_version({{Major, -1, -1, ""}, ""}, minor)) }
    else if
        dec_unsigned_int(Major),
        ['.'],
        dec_unsigned_int(Minor),
        ['.', '*'],
        ws_opt
    then
        { Range = w(wildcard_version({{Major, Minor, -1, ""}, ""}, patch)) }
    else
        { fail }
    ).

range_to_doc(Range) = str("<range_to_doc:NYI>").

invalid_range = v({{-1,0,0,""},"<invalid range>"}).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.semver.range.
%----------------------------------------------------------------------------%
