%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: package.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Sun Jan 25 15:51:50 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% The `package' modules provides the `package' type, the main reference to
% implement dependencies.
%
% NOTE: Unless the stability reaches 'high', you should refrain from using
% this code in production, as the on-disk file format for packages and the
% tuple to represent the `package' type might change.
%----------------------------------------------------------------------------%

:- module mercury_mpm.package.

:- interface.

:- import_module mercury_mpm.semver.

:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module univ.

%----------------------------------------------------------------------------%
%
% Types representing packages.
%
% As long as these types are not in the standard library, they have to
% be public, otherwise all libraries would have to depend on the
% `mercury_mpm' library, which is not what we want.
%

:- type packages == list(package).

:- type package == {string, version, dependencies}.

:- type dependencies == list(dependency).

    % The dependent `package' reference is stored in a `univ' since we cannot
    % use recursive type aliases.
    %
:- type dependency == {string, string, univ}.

    % A `package_file' wraps a URI, the `package' reference and other
    % parsed data (build instructions, etc.)
    %
:- type package_file
    --->    package_file(
                pkg_file_uri        :: string,
                pkg_file_package    :: package
            ).

%----------------------------------------------------------------------------%
%
% Package information access functions.
%
% Please use these instead of unpacking the `package' tuple directly.
%

    % Access package name
:- func package ^ pkg_name = string.

    % Access package version
:- func package ^ pkg_version = version.

    % dependencies(Package) = Dependencies:
    %
:- func dependencies(package) = dependencies.

    % resolve_dependencies(Package) = Dependencies:
    %
:- func resolve_dependencies(package) = packages.

%----------------------------------------------------------------------------%
%
% Package dependency information access functions.
%
% Please use these instead of unpacking the `dependency' tuple directly.
%

    % Access dependency name
:- func dependency ^ dep_name = string.

    % Access dependency version pattern
:- func dependency ^ dep_pattern = string.

    % Access dependent package
    %
    % This function might throw exceptions since the current implementation is
    % based on `univ', see the comment about stability on top of the module.
    %
:- func dependency ^ dep_package = package.

%----------------------------------------------------------------------------%

    % package_to_doc(Package) = Doc:
    %
:- func package_to_doc(package) = doc.

    % package_to_doc(Package) = Doc:
    %
    % Recursive version of `package_to_doc'/1.
    %
:- func package_tree_to_doc(package) = doc.

%----------------------------------------------------------------------------%

    % from_file(FileName, PackageFile, !IO):
    %
    % `PackageFile' is parsed from the given `FileName'.
    %
:- pred from_file(string::in, package_file::out, io::di, io::uo) is det.

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

:- import_module dir.       % for `det_basename'/1
:- import_module list.
:- import_module string.    % for `det_remove_suffix'/2

%----------------------------------------------------------------------------%

{Name, _, _} ^ pkg_name = Name.

{_, Version, _} ^ pkg_version = Version.

dependencies({_, _, Dependencies}) = Dependencies.

%----------------------------------------------------------------------------%

{Name, _, _} ^ dep_name = Name.

{_, Pattern, _} ^ dep_pattern = Pattern.

{_, _, Univ} ^ dep_package = Package :-
    (
        univ_to_type(Univ, Fun) ->
        Package = apply(Fun)
    ;
        det_univ_to_type(Univ, Package)
    ).

%----------------------------------------------------------------------------%

resolve_dependencies(Package) =
    map(dep_package, Package ^ dependencies).

%----------------------------------------------------------------------------%

package_to_doc(Package) =
    group([ str(Package ^ pkg_name)
          , str(": ")
          , version_to_doc(Package ^ pkg_version)
          ]).

package_tree_to_doc(Package) = Doc :-
    Dependencies = resolve_dependencies(Package),
    Doc = ( Dependencies = [] ->
        package_to_doc(Package)
    ;
        indent([package_to_doc(Package), nl |
                map(package_tree_to_doc, Dependencies)])
    ).

%----------------------------------------------------------------------------%

    % TODO: This is just a stub
from_file(FileName, package_file(FileName, {PkgName, Version, Deps}), !IO) :-
    PkgName = det_remove_suffix(det_basename(FileName), package_file_ext),
    Version = invalid_package_version,
    Deps = [{"libdepend", "0.[1-9].*",
        univ({"libdepend", invalid_package_version, []} : package)}].

package_file_ext = ".package".

package_file_to_doc(PackageFile) = docs(
    [indent([ str(PackageFile ^ pkg_file_uri)
            , nl
            , package_tree_to_doc(PackageFile ^ pkg_file_package)
            ])
    , nl % this ensures that a list of `package_file' is correctly nested
    ]).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.package.
%----------------------------------------------------------------------------%
