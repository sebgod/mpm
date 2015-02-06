%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: uri.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Tue Feb  3 11:43:03 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% An implementation of http://tools.ietf.org/html/rfc3986.
%----------------------------------------------------------------------------%

:- module mercury_mpm.uri.

:- interface.

:- import_module mercury_mpm.documentation.     % for `doc_ref(T)'

:- import_module pretty_printer.    % for `doc'

%----------------------------------------------------------------------------%

:- type uri.

:- typeclass uri(T)  where [
    (func to_uri(T) = uri)
].

:- instance docable(uri).
:- instance doc_ref(uri).

:- instance uri(string).
:- instance uri(uri).

:- func uri_to_doc(uri) = doc.

:- func uri_to_string(uri) = string.

    % uri_to_local_path(Uri, LocalPath):
    %
    % Succeeds iff `Uri' can be mapped to a local `LocalPath' and vice versa,
    % e.g.: file:///etc/fstab  <->  /etc/fstab
    %
    % NOTE: The `LocalPath' is OS-specific, see `dir.directory.separator'/0 in
    % dir.m.
    %
:- pred uri_to_local_path(uri, string).
:- mode uri_to_local_path(in, out) is semidet.
:- mode uri_to_local_path(out, in) is semidet.

    % det_uri_to_local_path(Uri) = LocalPath:
    %
    % Deterministic and functional versions of `uri_to_local_path'/2 access
    % modes, which throw an exception if the path/URI cannot be converted.
    %
:- func det_local_path_to_uri(string) = uri.
:- func det_uri_to_local_path(uri) = string.

    % local_path_to_uri(LocalPath, Uri):
    %
    % same as `uri_to_local_path(Uri, LocalPath)'.
    %
:- pred local_path_to_uri(string, uri).
:- mode local_path_to_uri(in, out) is semidet.
:- mode local_path_to_uri(out, in) is semidet.

    % basename(Uri) = BaseName.
    %
    % Returns the non-directory part of an `Uri'.
    %
    % Trailing slashes are removed from PathName before splitting,
    % if that doesn't change the meaning of PathName.
    %
:- func basename(uri) = string is semidet.
:- pred basename(uri::in, string::out) is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func det_basename(uri) = string.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mpm.dcg_parsing. % for `percent_encode_path_chars'/2

:- import_module dir.           % for `path_name_is_absolute'/1
:- import_module list.          % for `[|]'/2
:- import_module require.       % for `func_error'/2
:- import_module std_util.      % for `id'/1
:- import_module string.        % for `++'/2

%----------------------------------------------------------------------------%

:- type uri
    --->    local_path(string)  % an unencoded, OS-specific path
    ;       file(string)        % a valid file URI
    ;       url(string).        % a valid http(s) URI

:- instance docable(uri) where [
    (func(to_doc/1) is uri_to_doc)
].

:- instance doc_ref(uri) where [
    (func(to_string/1) is uri_to_string)
].

:- instance uri(uri) where [
    (func(to_uri/1) is std_util.id)
].

uri_to_doc(Uri) = str(uri_to_string(Uri)).

uri_to_string(local_path(LocalPath)) =
    "file:///" ++ percent_encode_path(LocalPath).

uri_to_string(file(FileUri))    = FileUri.
uri_to_string(url(Url))         = Url.

:- func percent_encode_path(string) = string.

percent_encode_path(Path) =
    ( if percent_encode_path_chars(to_char_list(Path), EncodedChars) then
        from_char_list(EncodedChars)
    else
        convert_local_path_func_error($pred, Path)
    ).

%----------------------------------------------------------------------------%
%
% Reference used for implementation:
% https://en.wikipedia.org/wiki/File_URI_scheme
%

uri_to_local_path(local_path(FilePath), FilePath) :-
    path_name_is_absolute(FilePath).

det_uri_to_local_path(Uri) =
    ( if uri_to_local_path(Uri, LocalPath) then
        LocalPath
    else
        convert_uri_func_error($pred, Uri)
    ).

local_path_to_uri(LocalPath, Uri) :-
    uri_to_local_path(Uri, LocalPath).

det_local_path_to_uri(LocalPath) =
    ( if uri_to_local_path(Uri, LocalPath) then
        Uri
    else
        convert_local_path_func_error($pred, LocalPath)
    ).

basename(Uri, BaseName) :-
    uri_to_local_path(Uri, LocalPath),
    basename(LocalPath, BaseName).

basename(Uri) = BaseName :-
    basename(Uri, BaseName).

det_basename(Uri) = det_basename(det_uri_to_local_path(Uri)).

%----------------------------------------------------------------------------%
%
% uri(T) typeclass implementation for `string'.
%

:- instance uri(string) where [
    (to_uri(UriOrPath) =
        ( if
            prefix(UriOrPath, "http:") ; prefix(UriOrPath, "https:")
        then
            url(UriOrPath)
        else if
            prefix(UriOrPath, "file:")
        then
            file(UriOrPath)  % TODO: Implement URI path decoding
        else
            det_local_path_to_uri(UriOrPath)
        )
    )
].

%----------------------------------------------------------------------------%
%
% Error handling functions.
%

:- func convert_uri_func_error(string, uri) = _ is erroneous.

convert_uri_func_error(Pred, Uri) =
    func_error(Pred,
        format("cannot convert `%s' to a local file path",
        [s(to_string(Uri))])).

:- func convert_local_path_func_error(string, string) = _ is erroneous.

convert_local_path_func_error(Pred, LocalPath) =
    func_error(Pred,
        format("cannot convert `%s' to a valid file URI",
        [s(LocalPath)])).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.uri.
%----------------------------------------------------------------------------%
