%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: container.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Thu 29 Jan 12:35:11 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% This module implements the `container' type.
% In this context a container is the collection of local package files
% stored in a source code repository (only 'git' is supported at the moment)
%----------------------------------------------------------------------------%

:- module mercury_mpm.container.

:- interface.

:- import_module io.
:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- type container.

:- pred find_container_up(string::in, res(container)::out, io::di, io::uo)
    is det.

    % container_to_doc(Container) = Doc:
    %
    % Lists all available modules together with their dependencies defined in
    % the given `Container'.
    %
:- func container_to_doc(container) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.formatting. % for `error_to_doc'/1
:- import_module mercury_mpm.package_file.
:- import_module mercury_mpm.resource.      % e.g. for `format_err_res'/4
:- import_module mercury_mpm.scm_repository.
:- import_module mercury_mpm.uri.           % used for package file paths

:- import_module bool.
:- import_module dir.
:- import_module list.
:- import_module maybe.
:- import_module pair.  % used for repo-dir
:- import_module string.

%----------------------------------------------------------------------------%
%
% Source code container resolution:
%
% The idea is that each package must reside within a version controlled
% source code repository, this simplifies package management (e.g. git clone).
%

:- type container
    --->    container(
                container_scm_repository    :: scm_repository,
                container_package_files     :: list(res(package_file))
            ).

find_container_up(DirName, ContainerRes, !IO) :-
    make_absolute(DirName, AbsDirNameRes, !IO),
    (
        AbsDirNameRes = ok(AbsDirName),
        foldl2(find_container_dir, AbsDirName, no, FindContainerDirRes, !IO),
        (
            FindContainerDirRes = ok(MaybeContainerDir),
            (
                MaybeContainerDir = yes(RepoKind-ContainerDir),
                foldl2(list_package_files, ContainerDir, [],
                    PackageFilesRes, !IO),
                (
                    PackageFilesRes = ok(PackageFiles),
                    init(RepoKind, ContainerDir, PackageFiles, Container,
                        !IO),
                    ContainerRes = ok(Container)
                ;
                    PackageFilesRes = error(_, Error),
                    ContainerRes = error(Error)
                )
            ;
                MaybeContainerDir = no,
                BaseDir = dirname(AbsDirName),
                ( if path_name_is_root_directory(BaseDir) then
                    ContainerRes = format_error_res(
                        "cannot go further up the file hierarchy: %s",
                        $file, $pred, [s(BaseDir)])
                else
                    find_container_up(BaseDir, ContainerRes, !IO)
                )
            )
        ;
            FindContainerDirRes = error(_, Error),
            ContainerRes = error(Error)
        )
    ;
        AbsDirNameRes = error(Error),
        ContainerRes = error(Error)
    ).

:- pred init(kind, T, list(uri), container, io, io) <= uri(T).
:- mode init(in, in, in, out, di, uo) is det.

init(RepoKind, ContainerDir, PackageFiles, Container, !IO) :-
    map_foldl(from_uri, PackageFiles, PackageFileResList, !IO),
    Repository = scm_repository(RepoKind, to_uri(ContainerDir)),
    Container = container(Repository, PackageFileResList).

:- pred list_package_files : foldl_pred(list(uri)) `with_inst` foldl_pred.

list_package_files(DirName, BaseName, FileType, yes, !PackageFiles, !IO) :-
    ( if
        FileType = regular_file, suffix(BaseName, package_file_ext),
        local_path_to_uri(DirName/BaseName, Uri)
    then
        !:PackageFiles = [Uri | !.PackageFiles]
    else
        true
    ).

:- pred find_container_dir : foldl_pred(maybe(pair(kind, string)))
    `with_inst` foldl_pred.

find_container_dir(DirName, BaseName, FileType, Continue, !Container, !IO) :-
    ( if FileType = directory, BaseName = ".git" then
        !:Container = yes(git-DirName),
        Continue = no
    else
        Continue = yes
    ).

%----------------------------------------------------------------------------%

container_to_doc(Container) = indent(
    [ scm_repository_to_doc(Container ^ container_scm_repository)
    , nl
    | map(
        (func(PackageFileRes) = Doc :-
            (
                PackageFileRes = ok(PackageFile),
                Doc = package_file_to_doc(PackageFile)
            ;
                PackageFileRes = error(Error),
                Doc = error_to_doc(Error)
            )
        ),
        Container ^ container_package_files)
    ]).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.container.
%----------------------------------------------------------------------------%
