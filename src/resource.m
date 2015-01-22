%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: resource.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Thu Jan 22 21:54:48 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module mercury_mpm.resource.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

    % progdir(ProgDir, !IO):
    %
    % Strips the name of the executable from `io.progname'/4.
    %
:- pred progdir(string::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.

%----------------------------------------------------------------------------%

progdir(ProgDir, !IO) :-
    io.progname("", ProgName, !IO),
    ProgDir = dirname(ProgName).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.resource.
%----------------------------------------------------------------------------%
