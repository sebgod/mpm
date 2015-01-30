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

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.documentation.
:- import_module mercury_mpm.command.
:- import_module mercury_mpm.meta_info.
:- import_module mercury_mpm.option.
:- import_module mercury_mpm.resource.
:- import_module mercury_mpm.container.
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
                Doc = docs(doc_ref_list_to_docs(ShowHelp, [Cmd]))
            ;
                ShowHelp = no,
                (
                    Cmd = list,
                    lookup_bool_option(OptionTable, installed, Installed),
                    (
                        Installed = yes,
                        Doc = str("listing installed packages")
                    ;
                        Installed = no,
                        find_container_up(this_directory, ContainerRes, !IO),
                        (
                            ContainerRes = ok(Container),
                            Doc = container_to_doc(Container)
                        ;
                            ContainerRes = io.error(Error),
                            Doc = error_to_doc(Error)
                        )
                    )
                ;
                    Cmd = build,
                    Doc = str("building local packages")
                )
            )
        else if ShowVersion = yes then
            Doc = package_tree_to_doc(ProgPackage)
        else
            Doc = prog_usage_to_doc(ShowHelp, ProgPackage)
        )
    ;
        Result = error(ErrorMessage),
        Doc = error_message_to_doc(ErrorMessage)
    ),
    write_doc(docs([Doc, nl]), !IO).

%----------------------------------------------------------------------------%

    % prog_usage_to_doc(Detailed, ProgPackage) = Doc:
    %
    % Documents the command line interface of the `ProgPackage' executable,
    % with detailed help for each command and option if `Detailed' is 'yes'.
    %
:- func prog_usage_to_doc(bool, package) = doc.

prog_usage_to_doc(Detailed, ProgPackage) = docs(Docs) :-
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
        indent([nl | OptionDocs])
    ].

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.cli.
%----------------------------------------------------------------------------%
