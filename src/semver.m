%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: semver.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Jan 21 13:54:35 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% This module implements the http://semver.org standard (2.0.0).
%----------------------------------------------------------------------------%

:- module mercury_mpm.semver.

:- interface.

%----------------------------------------------------------------------------%

:- type version
    --->    version(
                version_major   :: int,
                version_minor   :: int,
                version_patch   :: int,
                version_pre     :: string,
                version_build   :: string
            ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.semver.
%----------------------------------------------------------------------------%