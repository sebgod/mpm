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
:- include_module mercury_mpm.documentation.
:- include_module mercury_mpm.listing.
:- include_module mercury_mpm.meta_info.
:- include_module mercury_mpm.option.
:- include_module mercury_mpm.package.
:- include_module mercury_mpm.resource.
:- include_module mercury_mpm.semver.

%----------------------------------------------------------------------------%

:- end_module mercury_mpm.
%----------------------------------------------------------------------------%
