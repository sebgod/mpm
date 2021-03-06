%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: mercury_mpm.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Jan 21 12:25:26 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Library interface for `mercury_mpm'.
%----------------------------------------------------------------------------%

:- module mercury_mpm.

:- interface.

:- include_module mercury_mpm.cli.
:- include_module mercury_mpm.command.
:- include_module mercury_mpm.container.
:- include_module mercury_mpm.dcg_parsing.
:- include_module mercury_mpm.formatting.
:- include_module mercury_mpm.meta_info.
:- include_module mercury_mpm.option.
:- include_module mercury_mpm.package.
:- include_module mercury_mpm.package_file.
:- include_module mercury_mpm.resource.
:- include_module mercury_mpm.scm_repository.
:- include_module mercury_mpm.semver.
:- include_module mercury_mpm.uri.

%----------------------------------------------------------------------------%

:- end_module mercury_mpm.
%----------------------------------------------------------------------------%
