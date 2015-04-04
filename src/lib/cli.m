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
% The main entry point is `cli_main'/5, which provides all public API
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

    % cli_main(ProgName, ProgPackage, Args, !IO):
    %
    % The reference implementation of the Mercury package manager
    % command line tool, where:
    %
    % `ProgName' is the name of the currently running program, usually
    % obtained via `io.progname'/4
    %
    % `ProgPackage' is the package reference to the currently running
    % executable
    %
    % `Args' is a list of valid command line arguments, usually obtained by
    % calling `io.command_line_arguments'/3
    %
:- pred cli_main(string::in, package::in, list(string)::in, io::di, io::uo)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.command.
:- import_module mercury_mpm.formatting.
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

cli_main(ProgName, ProgPackage, Args, !IO) :-
    progroot(ProgName, ProgRootRes, !IO),
    progexe(ProgName, ProgExe, !IO),
    (
        ProgRootRes = ok(ProgRoot),
        OptionOps = option_ops_multi(short_option, long_option,
            option_default),
        process_options(OptionOps, Args, ProcessedArgs, Result),
        (
            Result = ok(OptionTable),

            lookup_bool_option(OptionTable, available, Available),
            lookup_bool_option(OptionTable, help, ShowHelp),
            lookup_bool_option(OptionTable, version, ShowVersion),
            lookup_bool_option(OptionTable, installed, Installed),
            lookup_maybe_string_or_default_option(OptionTable, install_prefix,
                ProgRoot, InstallPrefix),
            ( if
                ShowVersion = no,
                parse_command(Command, ProcessedArgs, CommandArgs)
            then
                (
                    ShowHelp = yes,
                    Action = show_command_usage(ShowHelp, Command)
                ;
                    ShowHelp = no,
                    (
                        Command = list,
                        (
                            Installed = yes,
                            Available = yes,
                            Action = show_error_message("cannot display " ++
                                "installed and available packages at the " ++
                                "same time")
                        ;
                            Installed = yes,
                            Available = no,
                            Action = list_installed_packages
                        ;
                            Installed = no,
                            Available = yes,
                            Action = list_available_packages
                        ;
                            Installed = no,
                            Available = no,
                            Action = list_current_container_packages
                        )
                    ;
                        Command = build,
                        ( if
                            CommandArgs = []
                        then
                            Action = build_current_container_packages
                        else
                            sorry($file, $pred, "building of single packages")
                        )
                    )
                )
            else if
                ShowVersion = yes
            then
                Action = show_package_tree(ProgPackage)
            else
                Action = show_proc_usage(ShowHelp, ProgExe)
            )
        ;
            Result = error(ErrorMessage),
            Action = show_error_message(ErrorMessage)
        )
    ;
        ProgRootRes = error(Error),
        Action = show_io_error(Error)
    ),
    Action(Doc, !IO),
    write_doc(Doc, !IO),
    ( if is_stderr(Doc) then
        io.set_exit_status(1, !IO)
    else
        true
    ).

:- type cli_pred == pred(doc_stream_nl_tuple, io, io).
:- inst cli_pred == (pred(out, di, uo) is det).

:- pred list_current_container_packages : cli_pred `with_inst` cli_pred.

list_current_container_packages(Doc, !IO) :-
    find_container_up(this_directory, ContainerRes, !IO),
    (
        ContainerRes = ok(Container),
        Doc = make_doc(container_to_doc(Container))
    ;
        ContainerRes = io.error(Error),
        Doc = make_doc(Error)
    ).

:- pred list_installed_packages : cli_pred `with_inst` cli_pred.

list_installed_packages(Doc, !IO) :-
    Doc = make_doc(str("listing installed packages")).

:- pred list_available_packages : cli_pred `with_inst` cli_pred.

list_available_packages(Doc, !IO) :-
    Doc = make_doc(str("listing available packages")).

:- pred build_current_container_packages : cli_pred `with_inst` cli_pred.

build_current_container_packages(Doc, !IO) :-
    Doc = make_doc(str("building local packages")).

:- pred show_package_tree(package::in) : cli_pred `with_inst` cli_pred.

show_package_tree(Package, Doc, !IO) :-
    Doc = make_doc(package_tree_to_doc(Package)).

    % show_proc_usage(Detailed, ProgPackage) = Doc:
    %
    % Documents the command line interface of the `ProgPackage' executable,
    % with detailed help for each command and option if `Detailed' is 'yes'.
    %
:- pred show_proc_usage(bool::in, string::in) : cli_pred `with_inst` cli_pred.

show_proc_usage(Detailed, ProgExe, Doc, !IO) :-
    CommandDocs =
        doc_ref_list_to_docs(Detailed, doc_ref_values : list(command)),
    OptionDocs =
        doc_ref_list_to_docs(Detailed, doc_ref_values : list(option)),
    Doc = make_docs([
        hard_nl,
        str(format("usage: %s ?[option] ?<command>", [s(ProgExe)])), hard_nl,
        hard_nl,
        str("where <command> is one of:"),
        indent([nl | CommandDocs]),
        hard_nl,
        str("where <option> is one of:"),
        indent([nl | OptionDocs])
    ]).

:- pred show_command_usage(bool::in, command::in) : cli_pred
    `with_inst` cli_pred.

show_command_usage(Detailed, Command, Doc, !IO) :-
    Doc = make_docs(doc_ref_list_to_docs(Detailed, [Command])).

:- pred show_error_message(string::in) : cli_pred `with_inst` cli_pred.

show_error_message(ErrorMessage, Doc, !IO) :-
    show_io_error(make_io_error(ErrorMessage), Doc, !IO).

:- pred show_io_error(io.error::in) : cli_pred `with_inst` cli_pred.

show_io_error(IOError, Doc, !IO) :-
    Doc = make_doc(IOError).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.cli.
%----------------------------------------------------------------------------%
