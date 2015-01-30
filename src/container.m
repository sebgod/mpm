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

:- import_module mercury_mpm.package.
:- import_module mercury_mpm.resource.

:- import_module bool.
:- import_module dir.
:- import_module list.
:- import_module maybe.
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
                container_scm_dir       :: string,
                container_package_files :: list(package_file)
            ).

find_container_up(DirName, ContainerRes, !IO) :-
    make_absolute(DirName, AbsDirNameRes, !IO),
    (
        AbsDirNameRes = ok(AbsDirName),
        foldl2(find_container_dir, AbsDirName, no, FindContainerDirRes, !IO),
        (
            FindContainerDirRes = ok(MaybeContainerDir),
            (
                MaybeContainerDir = yes(ContainerDir),
                foldl2(list_package_files, ContainerDir, [],
                    PackageFileNamesRes, !IO),
                (
                    PackageFileNamesRes = ok(PackageFileNames),
                    map_foldl(from_file, PackageFileNames, PackageFiles, !IO),
                    ContainerRes = ok(container(ContainerDir, PackageFiles))
                ;
                    PackageFileNamesRes = error(_, Error),
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

:- pred list_package_files : foldl_pred(list(string)) `with_inst` foldl_pred.

list_package_files(_DirName, BaseName, FileType, yes, !Files, !IO) :-
    ( if FileType = regular_file, suffix(BaseName, package_file_ext) then
        !:Files = [BaseName | !.Files]
    else
        true
    ).

:- pred find_container_dir : foldl_pred(maybe(string)) `with_inst` foldl_pred.

find_container_dir(DirName, BaseName, FileType, Continue, !Container, !IO) :-
    ( if FileType = directory, BaseName = ".git" then
        !:Container = yes(DirName),
        Continue = no
    else
        Continue = yes
    ).

%----------------------------------------------------------------------------%

container_to_doc(Container) = indent(
    [ str("git repository: ")
    , str(Container ^ container_scm_dir)
    , nl
    | map(package_file_to_doc, Container ^ container_package_files)
    ]).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.container.
%----------------------------------------------------------------------------%
