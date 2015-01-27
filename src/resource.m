%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: resource.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Thu Jan 22 21:54:48 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% An abstraction of all resources (file storage, fixed URIs, etc).
%----------------------------------------------------------------------------%

:- module mercury_mpm.resource.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

    % progexe(ProgName, ProgExe, !IO):
    %
    % `ProgExe' is the name of the program `ProgName' with stripped
    % directory part.
    %
    % Fails if the basename could not be obtained.
    %
:- pred progexe(string::in, string::out, io::di, io::uo) is det.

    % progdir(ProgName, ProgDir, !IO):
    %
    % `ProgDir' is the directory part of the executable `ProgName'.
    %
:- pred progdir(string::in, string::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

progexe(ProgName, ProgExe, !IO) :-
    ( BaseName = basename(ProgName) ->
        ProgExe = BaseName
    ;
        unexpected($module, $pred,
            "failed to obtain the basename of " ++ ProgName)
    ).

progdir(ProgName, dirname(ProgName), !IO).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.resource.
%----------------------------------------------------------------------------%
