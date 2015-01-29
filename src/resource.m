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

%----------------------------------------------------------------------------%

:- type repository
    --->    repository(
                repo_dir            :: string,
                repo_package_files  :: list(string)
            ).

:- pred find_repository_up(string::in, res(repository)::out, io::di, io::uo)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module maybe.
:- import_module require.
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
% Source code repository resolution:
%
% The idea is that each package must reside within a version controlled
% repository, this simplifies package management (i.e. to cloning)
%

find_repository_up(DirName, RepoRes, !IO) :-
    make_absolute(DirName, AbsDirNameRes, !IO),
    (
        AbsDirNameRes = ok(AbsDirName),
        foldl2(find_repo_dir, AbsDirName, no, FindRepoDirRes, !IO),
        (
            FindRepoDirRes = ok(MaybeRepoDir),
            (
                MaybeRepoDir = yes(RepoDir),
                foldl2(list_package_files, RepoDir, [], PackageFilesRes, !IO),
                (
                    PackageFilesRes = ok(PackageFiles),
                    RepoRes = ok(repository(RepoDir, PackageFiles))
                ;
                    PackageFilesRes = error(_, Error),
                    RepoRes = error(Error)
                )
            ;
                MaybeRepoDir = no,
                BaseDir = dirname(AbsDirName),
                ( if path_name_is_root_directory(BaseDir) then
                    RepoRes = format_error_res("cannot go further up: %s",
                        $file, $pred, [s(BaseDir)])
                else
                    find_repository_up(BaseDir, RepoRes, !IO)
                )
            )
        ;
            FindRepoDirRes = error(_, Error),
            RepoRes = error(Error)
        )
    ;
        AbsDirNameRes = error(Error),
        RepoRes = error(Error)
    ).

:- pred list_package_files : foldl_pred(list(string)) `with_inst` foldl_pred.

list_package_files(_DirName, BaseName, FileType, yes, !Files, !IO) :-
    ( if FileType = regular_file, suffix(BaseName, ".package") then
        !:Files = [BaseName | !.Files]
    else
        true
    ).

:- pred find_repo_dir : foldl_pred(maybe(string)) `with_inst` foldl_pred.

find_repo_dir(DirName, BaseName, FileType, Continue, !Repo, !IO) :-
    ( if FileType = directory, BaseName = ".git" then
        !:Repo = yes(DirName),
        Continue = no
    else
        Continue = yes
    ).

%----------------------------------------------------------------------------%
%
% `io.error' format helper functions
%

:- func format_error_res(string, string, string, list(io.poly_type)) =
    res(T).

format_error_res(Fmt, File, Pred, Params) =
    error(format_error(Fmt, File, Pred, Params)).

:- func format_error(string, string, string, list(io.poly_type)) = io.error.

format_error(Fmt, File, Pred, Params) =
    make_io_error(format("%s: %s: " ++ Fmt, [s(File), s(Pred) | Params])).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.resource.
%----------------------------------------------------------------------------%
