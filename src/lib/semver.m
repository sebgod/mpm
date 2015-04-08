%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: semver.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Jan 21 13:54:35 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Namespace for semver (http://semver.org) related functionality,
%  * semver versions in semver.version.m
%  * version ranges in semver.range.m
%----------------------------------------------------------------------------%

:- module mercury_mpm.semver.

:- interface.

:- include_module semver.version.
:- include_module semver.range.

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.semver.
%----------------------------------------------------------------------------%
