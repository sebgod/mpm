%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: semver.range.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Apr  8 12:37:23 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module mercury_mpm.semver.range.

:- interface.

:- import_module pretty_printer.

%----------------------------------------------------------------------------%

    % A version range, e.g. 1.2.* or 1.2.10-1.3.*
    %
:- type range == string.

    % det_string_to_range(RangeString) = Range:
    %
    % Builds `Range' by parsing the `RangeString' or throws an exception
    % iff `RangeString' is not a valid range.
    %
:- func det_string_to_range(string) = range.

:- func range_to_doc(range) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.dcg_parsing.
:- import_module mercury_mpm.formatting.

:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

    % TODO: Do real parsing and define a range representation.
det_string_to_range(RangeString) = RangeString.

range_to_doc(RangeString) = str(RangeString).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.semver.range.
%----------------------------------------------------------------------------%
