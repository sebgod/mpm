%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: scm_repository.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Fri Feb  6 16:28:09 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Repository represents a source code scm_repository.
% At the moment only 'git' repositories are supported, see `kind' type.
%----------------------------------------------------------------------------%

:- module mercury_mpm.scm_repository.

:- interface.

:- import_module mercury_mpm.formatting.
:- import_module mercury_mpm.uri.

:- import_module pretty_printer.

%----------------------------------------------------------------------------%

:- type scm_repository
    --->    scm_repository(
                scm_repo_kind   :: kind,
                scm_repo_uri    :: uri
            ).

:- instance docable(scm_repository).

:- type kind
    ---> git.

:- func scm_repository_to_doc(scm_repository) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.uri.

:- import_module list.  % for `[|]'/2

%----------------------------------------------------------------------------%

:- instance docable(scm_repository) where [
    func(to_doc/1) is scm_repository_to_doc
].

scm_repository_to_doc(Repository) =
    group([ format(Repository ^ scm_repo_kind)
          , str("+")
          , uri_to_doc(Repository ^ scm_repo_uri)
          ]).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.scm_repository.
%----------------------------------------------------------------------------%
