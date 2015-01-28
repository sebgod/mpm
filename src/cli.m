%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: cli.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Wed Jan 21 13:00:13 CST 2015
% Stability: medium
%----------------------------------------------------------------------------%
% This module implements the command line interface for the `mercury_mpm'
% library.
%
% The main entry point is `cli_main'/4, which provides all public API
% functionality through a command line interface.
% This helps to loosely embed package management functionality into other
% applications without fully implementing all `mercury_mpm' functionality.
%----------------------------------------------------------------------------%

:- module mercury_mpm.cli.

:- interface.

:- import_module mercury_mpm.package.

:- import_module io.
:- import_module list.

%----------------------------------------------------------------------------%

    % cli_main(ProgPackage, Args, !IO):
    %
    % The reference implementation of the Mercury package manager
    % command line tool, where:
    %
    % `ProgPackage' is the package reference to the currently running
    % executable.
    %
    % `Args' is a list of valid command line arguments, usually obtained by
    % calling `io.command_line_arguments'/3.
    %
:- pred cli_main(package::in, list(string)::in, io::di, io::uo) is det.

    % show_version(ProgPackage, !IO):
    %
    % This procedure will show the name and version of the
    % executing program given as `ProgPackage' and of all of its dependencies.
    %
:- pred show_version(package::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.command.
:- import_module mercury_mpm.meta_info.
:- import_module mercury_mpm.resource.
:- import_module mercury_mpm.semver.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module pretty_printer.
:- import_module require.
:- import_module solutions.
:- import_module string.

%----------------------------------------------------------------------------%

cli_main(ProgPackage, Args, !IO) :-
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    process_options(OptionOps, Args, ProcessedArgs, Result),
    (
        Result = ok(OptionTable),

        lookup_bool_option(OptionTable, help, ShowHelp),
        lookup_bool_option(OptionTable, version, ShowVersion),
        ( if
            ShowVersion = no,
            parse_cmd(Cmd, ProcessedArgs, _CmdArgs)
        then
            (
                ShowHelp = yes,
                show_cmd_usage(ShowHelp, [{Cmd, to_string(Cmd)}], !IO)
            ;
                ShowHelp = no,
                (
                    Cmd = list,
                    print_line("listing dependencies", !IO)
                ;
                    Cmd = build,
                    print_line("building local packages", !IO)
                )
            )
        else if ShowVersion = yes then
            show_version(ProgPackage, !IO)
        else
            show_usage(ShowHelp, ProgPackage, !IO)
        )
    ;
        Result = error(ErrorMessage),
        require.error(ErrorMessage)
    ).

%----------------------------------------------------------------------------%

show_version(ProgPackage, !IO) :-
    write_doc(docs([package_tree_to_doc(ProgPackage), nl]), !IO).

%----------------------------------------------------------------------------%

    % show_usage(Detailed, ProgPackage, !IO):
    %
    % Displays the command line interface using the `ProgPackage' as the
    % executable, with detailed help for each command if `Detailed' is 'yes'.
    %
:- pred show_usage(bool::in, package::in, io::di, io::uo) is det.

show_usage(Detailed, ProgPackage, !IO) :-
    solutions(
        (pred({Cmd, CmdString}::out) is nondet :-
            parse_cmd(Cmd, [CmdString], [])
        ),
        CmdRefs
    ),
    CmdDocs = cmd_usage(Detailed, CmdRefs),
    Name = ProgPackage ^ pkg_name,
    Docs = [
        hard_nl,
        str(format("usage: %s ?[options] <command>", [s(Name)])), hard_nl,
        hard_nl,
        str("where <command> is one of:"),
        indent([nl | CmdDocs]),
        hard_nl,
        str("where <option> is one of:"), nl
    ],
    write_doc(docs(Docs), !IO).

    % show_cmd_usage(Detailed, CmdRefs, !IO):
    %
    % Displays command line usage information on all given command references.
    % This is used by the general `show_usage'/4 and by the `-h' flag
    % implementation.
    %
:- pred show_cmd_usage(bool::in, cmd_refs::in, io::di, io::uo) is det.

show_cmd_usage(Detailed, CmdRefs, !IO) :-
    write_doc(docs(cmd_usage(Detailed, CmdRefs)), !IO).

:- func cmd_usage(bool, cmd_refs) = docs.

cmd_usage(Detailed, CmdRefs) = CmdDocs :-
    (
        Detailed = yes,
        CmdFmt =
            (func({Cmd, CmdString}) =
                group([
                    str(CmdString), str(" - "),
                    indent(cmd_to_docs(Cmd)),
                    hard_nl
                ])
            ),
        OptNl = []
    ;
        Detailed = no,
        CmdFmt = (func({_, CmdString}) = docs([str(CmdString), str(" ")])),
        OptNl = [nl]
    ),
    CmdDocs = map(CmdFmt, CmdRefs) ++ OptNl.

%----------------------------------------------------------------------------%

:- type option
    --->    help        % displays help about program usage.
    ;       debug       % enables debugging of package commands.
    ;       version     % displays the versions of the library and executable.
    .

:- pred short_option(char::in, option::out) is semidet.

short_option(h, help).
short_option(d, debug).
short_option(v, version).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("debug", debug).
long_option("version", version).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,    bool(no)).
option_default(debug,   bool(no)).
option_default(version, bool(no)).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.cli.
%----------------------------------------------------------------------------%
