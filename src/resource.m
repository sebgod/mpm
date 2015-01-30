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
:- import_module list.

%----------------------------------------------------------------------------%

    % progexe(ProgName, ProgExe, !IO):
    %
    % `ProgExe' is the name of the program `ProgName' with stripped
    % directory part.
    %
    % Fails if the `io.basename'/1 could not be obtained.
    %
:- pred progexe(string::in, string::out, io::di, io::uo) is det.

    % progdir(ProgName, ProgDir, !IO):
    %
    % `ProgDir' is the directory part of the executable `ProgName'.
    %
:- pred progdir(string::in, string::out, io::di, io::uo) is det.

    % `resolve_dir_pred' is for resolving special directories
    % like '.' or '..' to enable a unified path handling.
    %
:- type resolve_dir_pred == pred(string, res(string), io, io).

:- inst resolve_dir_pred == (pred(in, out, di, uo) is det).

    % make_absolute(DirName, AbsDirNameRes, !IO):
    %
    % Tries to resolve a possibly relative input directory `DirName'
    % into an `io.res' result `AbsDirNameRes', while preserving all
    % I/O errors.
    %
:- pred make_absolute : resolve_dir_pred `with_inst` resolve_dir_pred.

    % format_error_res(Fmt, File, Pred, Params) = Result:
    %
    % Creates a `io.res' result with the `error' constructor initalised with
    % a formatted error message, obtained using:
    % format_error(Fmt, File, Pred, Params)
    %
:- func format_error_res(string, string, string, list(io.poly_type)) =
    res(T).

    % format_error(Fmt, File, Pred, Params) = Error:
    %
    % Uses `Fmt' and `Params' to create a formatted `io.error'
    % `Error' message, including the `File' and `Pred' name.
    %
:- func format_error(string, string, string, list(io.poly_type)) = io.error.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module string.

%----------------------------------------------------------------------------%
%
% File path management:
%

progexe(ProgName, ProgExe, !IO) :-
    ProgExe = det_basename(ProgName).

progdir(ProgName, dirname(ProgName), !IO).

make_absolute(DirName, AbsDirNameRes, !IO) :-
    resolve_special(DirName, NoSpecialRes, !IO),
    ( if
        NoSpecialRes = ok(NoSpecialDirName),
        path_name_is_absolute(NoSpecialDirName)
    then
        AbsDirNameRes = NoSpecialRes
    else if
        NoSpecialRes = ok(NoSpecialDirName)
    then
        AbsDirNameRes = error(make_io_error("cannot make absolute: " ++
            NoSpecialDirName))
    else
        AbsDirNameRes = NoSpecialRes
    ).

:- pred resolve_special : resolve_dir_pred `with_inst` resolve_dir_pred.

resolve_special(DirName, NoSpecialRes, !IO) :-
    ( if
        DirName = this_directory
    then
        current_directory(NoSpecialRes, !IO)
    else if
        DirName = parent_directory
    then
        current_directory(ParentDirRes, !IO),
        (
            ParentDirRes = ok(ParentDir),
            NoSpecialRes = ok(dirname(ParentDir))
        ;
            ParentDirRes = error(Error),
            NoSpecialRes = error(Error)
        )
    else
        NoSpecialRes = ok(DirName)
    ).


%----------------------------------------------------------------------------%
%
% Implementation of `io.error' format helper functions
%

format_error_res(Fmt, File, Pred, Params) =
    error(format_error(Fmt, File, Pred, Params)).

format_error(Fmt, File, Pred, Params) =
    make_io_error(format("%s: %s: " ++ Fmt, [s(File), s(Pred) | Params])).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.resource.
%----------------------------------------------------------------------------%
