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

% :- import_module mercury_mpm.listing.
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

:- import_module mercury_mpm.documentation.
:- import_module mercury_mpm.command.
:- import_module mercury_mpm.meta_info.
:- import_module mercury_mpm.option.
:- import_module mercury_mpm.resource.
:- import_module mercury_mpm.semver.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module getopt.
:- import_module pretty_printer.
:- import_module require.
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
                show_usages(ShowHelp, [Cmd], !IO)
            ;
                ShowHelp = no,
                (
                    Cmd = list,
                    print_line("listing dependencies", !IO)
                    % find_repository_up(this_directory, RepoRes, !IO)
                ;
                    Cmd = build,
                    print_line("building local packages", !IO)
                )
            )
        else if ShowVersion = yes then
            show_version(ProgPackage, !IO)
        else
            show_prog_usage(ShowHelp, ProgPackage, !IO)
        )
    ;
        Result = error(ErrorMessage),
        require.error(ErrorMessage)
    ).

%----------------------------------------------------------------------------%

show_version(ProgPackage, !IO) :-
    write_doc(docs([package_tree_to_doc(ProgPackage), nl]), !IO).

%----------------------------------------------------------------------------%

    % show_prog_usage(Detailed, ProgPackage, !IO):
    %
    % Displays the command line interface using the `ProgPackage' as the
    % executable, with detailed help for each command if `Detailed' is 'yes'.
    %
:- pred show_prog_usage(bool::in, package::in, io::di, io::uo) is det.

show_prog_usage(Detailed, ProgPackage, !IO) :-
    CmdDocs =
        doc_ref_list_to_docs(Detailed, doc_ref_values : list(cmd)),
    OptionDocs =
        doc_ref_list_to_docs(Detailed, doc_ref_values : list(option)),
    Name = ProgPackage ^ pkg_name,
    Docs = [
        hard_nl,
        str(format("usage: %s ?[option] ?<command>", [s(Name)])), hard_nl,
        hard_nl,
        str("where <command> is one of:"),
        indent([nl | CmdDocs]),
        hard_nl,
        str("where <option> is one of:"),
        indent([nl | OptionDocs]),
        hard_nl
    ],
    write_doc(docs(Docs), !IO).

    % show_usages(Detailed, DocRefs, !IO):
    %
    % Displays command line usage information on all given `DocRefs' commands
    % or options, with optional `Detailed' level.
    %
    % This is used by the general `show_usage'/4 and by the `-h' flag
    % implementation.
    %
:- pred show_usages(bool::in, list(T)::in, io::di, io::uo) is det
    <= doc_ref(T).

show_usages(Detailed, DocRefs, !IO) :-
    write_doc(docs(doc_ref_list_to_docs(Detailed, DocRefs)), !IO).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.cli.
%----------------------------------------------------------------------------%
