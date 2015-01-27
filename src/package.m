%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: package.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Sun Jan 25 15:51:50 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% The `package' modules provides the `package(T)' type, the main reference to
% implement dependencies.
%----------------------------------------------------------------------------%

:- module mercury_mpm.package.

:- interface.

:- import_module mercury_mpm.semver.

:- import_module list.
:- import_module pretty_printer.
:- import_module univ.

%----------------------------------------------------------------------------%

:- type dependencies == list(dependency).

:- type dependency == {string, string, univ}.

:- type packages == list(package).

:- type package == {string, version, dependencies}.

:- func package ^ name = string.

:- func package ^ version = version.

:- func package ^ dependencies = dependencies.

:- func package_to_doc(package) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

{Name, _, _} ^ name = Name.

{_, Version, _} ^ version = Version.

{_, _, Dependencies} ^ dependencies = Dependencies.

%----------------------------------------------------------------------------%

package_to_doc(Package) =
    docs(
        [group([str(Package ^ name)
               ,str(": ")
               ,version_to_doc(Package ^ version)
               ])
        ,nl
        ]
    ).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.package.
%----------------------------------------------------------------------------%
