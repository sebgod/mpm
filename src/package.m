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
%----------------------------------------------------------------------------%

:- module mercury_mpm.package.

:- interface.

:- import_module mercury_mpm.semver.

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

%----------------------------------------------------------------------------%

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

    % Access dependency name
:- func dependency ^ dep_name = string.

    % Access dependency version pattern
:- func dependency ^ dep_pattern = string.

    % Access dependent package
:- func dependency ^ dep_package = package.

%----------------------------------------------------------------------------%

    % package_to_doc(Package) = Doc:
    %
:- func package_to_doc(package) = doc.

    % package_to_doc(Package) = Doc:
    %
    % Recursive version of `package_to_doc'/1.
:- func package_tree_to_doc(package) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

{Name, _, _} ^ pkg_name = Name.

{_, Version, _} ^ pkg_version = Version.

dependencies({_, _, Dependencies}) = Dependencies.

%----------------------------------------------------------------------------%

{Name, _, _} ^ dep_name = Name.

{_, Pattern, _} ^ dep_pattern = Pattern.

{_, _, Univ} ^ dep_package = Package :-
    det_univ_to_type(Univ, Package).

%----------------------------------------------------------------------------%

resolve_dependencies(Package) =
    map(dep_package, Package ^ dependencies).

%----------------------------------------------------------------------------%

package_to_doc(Package) =
    group([str(Package ^ pkg_name)
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
:- end_module mercury_mpm.package.
%----------------------------------------------------------------------------%
