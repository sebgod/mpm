%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: ansi_escape_code.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Sun Feb 15 23:08:20 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Implement ANSI SGR (select graphics rendition) escape codes, please see
% https://en.wikipedia.org/wiki/ANSI_escape_code for details.
%
% TODO: Check if we are actually on an ANSI escape code compatible console,
% under Linux this should be almost a given, under Windows check if the
% CON_EMU variable is set.
% TODO: Maybe merge coloured_pretty_printer.m functionality here.
%
%----------------------------------------------------------------------------%

:- module mercury_mpm.formatting.ansi_escape_code.

:- interface.

:- import_module enum.          % used for `instance(ansi_sgr_code)'

%----------------------------------------------------------------------------%

:- type ansi_sgr
    --->    bold
    ;       not_bold
    ;       red
    ;       default_colours
    .

:- instance enum(ansi_sgr).

:- func enclose_with_ansi_sgr_pair(string, ansi_sgr) = string.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.       % used for `ansi_sgr_pair'/1

%----------------------------------------------------------------------------%

enclose_with_ansi_sgr_pair(String, SGR) = format("\x1b\[%dm%s\x1b\[%dm",
    [i(to_int(SGR)), s(String), i(to_int(ansi_sgr_pair(SGR)))]).

:- instance enum(ansi_sgr) where [
    (from_int(IntVal) = SGR    :- ansi_sgr_code(SGR, IntVal)),
    (to_int(SGR)      = IntVal :- ansi_sgr_code(SGR, IntVal))
].

:- pred ansi_sgr_code(ansi_sgr, int).
:- mode ansi_sgr_code(in, out) is det.
:- mode ansi_sgr_code(out, in) is semidet.

ansi_sgr_code(bold, 1).
ansi_sgr_code(not_bold, 22).
ansi_sgr_code(red, 31).
ansi_sgr_code(default_colours, 39).

:- func ansi_sgr_pair(ansi_sgr) = ansi_sgr.

ansi_sgr_pair(SGR) =
    ( if ansi_sgr_pair(SGR, Pair) then
        Pair
    else
        unexpected($file, $pred, format("ANSI SGR %d has no pair!",
            [i(to_int(SGR))]))
    ).

:- pred ansi_sgr_pair(ansi_sgr, ansi_sgr).
:- mode ansi_sgr_pair(in, out) is semidet.

ansi_sgr_pair(bold, not_bold).
ansi_sgr_pair(red, default_colours).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.formatting.ansi_escape_code.
%----------------------------------------------------------------------------%
