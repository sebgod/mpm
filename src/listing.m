%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: listing.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Thu 29 Jan 12:35:11 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% This module implements listing of installed or local packages including
% their dependence tree.
%----------------------------------------------------------------------------%

:- module mercury_mpm.listing.

:- interface.

:- import_module mercury_mpm.resource.

:- import_module pretty_printer.

%----------------------------------------------------------------------------%

    % list_repo_packages(Repository) = doc:
    %
    % Lists all avaiable modules together with their dependencies defined in
    % the given `Repository'.
    %
:- func list_repo_packages(repository) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

list_repo_packages(_Repo) = str("?repo?").

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.listing.
%----------------------------------------------------------------------------%
