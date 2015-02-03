%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: package_file.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Mon  2 Feb 09:40:29 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% The package file module deals with parsing and resolving '.package' files.
%----------------------------------------------------------------------------%

:- module mercury_mpm.package_file.

:- interface.

:- import_module mercury_mpm.package.
:- import_module mercury_mpm.uri.

:- import_module io.
:- import_module pretty_printer.

%----------------------------------------------------------------------------%

    % A `package_file' wraps a file name, the `package' reference and other
    % parsed data (authors, build instructions, etc.)
    %
:- type package_file
    --->    package_file(
                pkg_file_uri        :: uri,
                pkg_file_package    :: package
            ).

    % from_uri(Uri, PackageFileRes, !IO):
    %
    % `PackageFileRes' is the parsed package file from the given `Uri'.
    %
:- pred from_uri(uri::in, res(package_file)::out, io::di, io::uo) is det.

    % package_file_ext = Extension:
    %
    % `Extension' is the extension used for Mercury package files,
    % in the current implementation this is fixed to '.package'.
    %
:- func package_file_ext = string.

    % package_file_to_doc(Package) = Doc:
    %
:- func package_file_to_doc(package_file) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- use_module mercury_mpm.semver.
:- import_module mercury_mpm.documentation. % for `doc_ref(T).to_string'/1
:- import_module mercury_mpm.resource.      % e.g. for `format_err_res'/4

:- import_module dir.       % for `det_basename'/1
:- import_module list.
:- import_module maybe.     % e.g. for representing optional package data.
:- import_module string.    % for `det_remove_suffix'/2
:- import_module term_conversion. % for `term_to_type'/2
:- import_module term_io.   % used for parsing package files
:- import_module univ.      % for `univ', used for package dependencies

%----------------------------------------------------------------------------%

:- inst package_file_res == unique(io.error(ground) ; io.ok(ground)).

:- type version
    ---> version(string).

:- type dep
    --->    dep(
                dep_name    :: string,
                dep_pattern :: string
            ).

:- func init(uri::in) = (package_file::out) is det.

init(Uri) =
    package_file(Uri,
        {PkgName, semver.invalid_package_version, []}) :-
    PkgName = det_remove_suffix(det_basename(Uri), package_file_ext).

%----------------------------------------------------------------------------%

from_uri(Uri, PackageFileRes, !IO) :-
    ( if uri_to_local_path(Uri, AbsFilePath) then
        from_file(AbsFilePath, init(Uri), PackageFileRes, !IO)
    else
        PackageFileRes =
            format_error_res("Cannot convert %s to local file path",
                $file, $pred, [s(to_string(Uri))])
    ).

    % from_file(AbsFilePath, PackageFileInit, PackageFileRes, !IO):
    %
:- pred from_file(string, package_file, res(package_file), io, io).
:- mode from_file(in, in, free >> package_file_res, di, uo) is det.

from_file(AbsFilePath, PackageFileInit, PackageFileRes, !IO) :-
    io.see(AbsFilePath, OpenRes, !IO),
    (
        OpenRes = ok,
        parse_loop(PackageFileInit, PackageFile, MaybeError, !IO),
        (
            MaybeError = no,
            PackageFileRes = ok(PackageFile)
        ;
            MaybeError = yes(Error),
            PackageFileRes = error(Error)
        )
    ;
        OpenRes = error(Error),
        PackageFileRes = error(Error)
    ),
    io.seen(!IO).

    % parse_loop(!PackageFile, !IO):
    %
:- pred parse_loop(package_file, package_file, maybe(io.error), io, io).
:- mode parse_loop(in, out, out, di, uo) is det.

parse_loop(!PackageFile, MaybeError, !IO) :-
    read_term(ReadTerm : read_term(string), !IO),
    (
        ReadTerm = term(_Varset, Term),
        ( if
            term_to_type(Term, version(VersionString))
        then
            !:PackageFile = !.PackageFile ^ pkg_file_package ^ pkg_version :=
                semver.det_string_to_version(VersionString)
        else if
            term_to_type(Term, Dep : dep)
        then
            !:PackageFile = !.PackageFile ^ pkg_file_package ^ pkg_deps :=
                [ { Dep ^ dep_name,
                    Dep ^ dep_pattern,
                    univ((func(Name, _Pattern : string) =
                            { Name
                            , semver.invalid_package_version
                            , []
                            } : package))
                  }
                | !.PackageFile ^ pkg_file_package ^ pkg_deps
                ]
        else
            true
        ),
        parse_loop(!PackageFile, MaybeError, !IO)
    ;
        ReadTerm = error(Msg, LineNumber),
        input_stream_name(FileName, !IO),
        MaybeError = yes(make_io_error(format("%s:%d: %s",
                            [s(FileName), i(LineNumber), s(Msg)])))
    ;
        ReadTerm = eof,
        MaybeError = no
    ).

%----------------------------------------------------------------------------%

package_file_ext = ".package".

package_file_to_doc(PackageFile) = docs(
    [indent([ to_doc(PackageFile ^ pkg_file_uri)
            , nl
            , package_tree_to_doc(PackageFile ^ pkg_file_package)
            ])
    , nl % this ensures that a list of `package_file' is correctly nested
    ]).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.package_file.
%----------------------------------------------------------------------------%