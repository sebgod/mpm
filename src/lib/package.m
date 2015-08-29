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
% `package' type might change.
%----------------------------------------------------------------------------%

:- module mercury_mpm.package.

:- interface.

:- import_module mercury_mpm.semver.
:- import_module mercury_mpm.semver.version.
:- import_module mercury_mpm.semver.range.

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

    % dependency(Name, Range, Package):
    %
    % The semver `Range' has the runtime type `string' or `range'.
    %
    % The dependent `Package' reference has the runtime type
    % `dependency_func' or `package'.
    %
    % NOTE: We use `univ' since we cannot use recursive type aliases.
    %
:- type dependency == {string, univ, univ}.

    % This higher-order function type is used for resolving package
    % dependencies dynamically.
    %
:- type dependency_func == (func(string, univ) = package).

%----------------------------------------------------------------------------%
%
% Package information access functions.
%
% Please use these instead of unpacking the `package' tuple directly.
%

    % Access package name
:- func package ^ pkg_name = string.
:- func 'pkg_name :='(package, string) = package.

    % Access package version
:- func package ^ pkg_version = version.
:- func 'pkg_version :='(package, version) = package.

    % Access package dependencies
:- func package ^ pkg_deps = dependencies.
:- func 'pkg_deps :='(package, dependencies) = package.

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

    % Access dependency version range
:- func dependency ^ dep_range = range.

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
%----------------------------------------------------------------------------%

:- implementation.

:- import_module list.

%----------------------------------------------------------------------------%
%
% Package access functions
%

{Name, _, _} ^ pkg_name = Name.

'pkg_name :='({_, Version, Deps}, Name) = {Name, Version, Deps}.

{_, Version, _} ^ pkg_version = Version.

'pkg_version :='({Name, _, Deps}, Version) = {Name, Version, Deps}.

({_, _, Dependencies}) ^ pkg_deps = Dependencies.

'pkg_deps :='({Name, Version, _}, Deps) = {Name, Version, Deps}.

%----------------------------------------------------------------------------%
%
% Dependency access functions
%

{Name, _, _} ^ dep_name = Name.

{_, RangeUniv, _} ^ dep_range = Range :-
    ( if univ_to_type(RangeUniv, RangeString) then
        Range = det_string_to_range(RangeString)
    else
        det_univ_to_type(RangeUniv, Range)
    ).

Dep @ {Name, _RangeUniv, PackageUniv} ^ dep_package = Package :-
    Range = Dep ^ dep_range,
    ( if univ_to_type(PackageUniv, PackageResolveFunction) then
        Package = PackageResolveFunction(Name, Range)
    else
        det_univ_to_type(PackageUniv, Package)
    ).

%----------------------------------------------------------------------------%

resolve_dependencies(Package) =
    map(dep_package, Package ^ pkg_deps).

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
:- end_module mercury_mpm.package.
%----------------------------------------------------------------------------%
