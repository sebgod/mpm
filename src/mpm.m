%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: mpm.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Jan 21 12:26:59 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% `mpm' command line interface similar to the https://www.npmjs.com tool.
%----------------------------------------------------------------------------%

:- module mpm.

:- interface.

:- include_module mpm.meta_info.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mpm.meta_info.

:- import_module mercury_mpm.
:- import_module mercury_mpm.cli.

%----------------------------------------------------------------------------%

main(!IO) :-
    command_line_arguments(Args, !IO),
    cli_main(package, Args, !IO).

%----------------------------------------------------------------------------%
:- end_module mpm.
%----------------------------------------------------------------------------%
