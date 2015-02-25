%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: dcg_parsing.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Mon  2 Feb 16:48:36 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Generic parsing helper for simple DCG-based character list parsing.
%----------------------------------------------------------------------------%

:- module mercury_mpm.dcg_parsing.

:- interface.

:- import_module char.
:- import_module list.

%----------------------------------------------------------------------------%

:- type chars == list(char).

:- type parser_pred     == pred(chars, chars).
:- type parser1_pred(T) == pred(T, chars, chars).
:- type parser2_pred(T) == pred(T, T, chars, chars).

%----------------------------------------------------------------------------%

:- pred dec_digit : parser1_pred(int).
:- mode dec_digit(uo, mdi, muo) is semidet.
:- mode dec_digit(uo, di, uo) is semidet.
:- mode dec_digit(uo, in, out) is semidet.

:- pred dec_unsigned_int : parser1_pred(int).
:- mode dec_unsigned_int(uo, di, muo) is semidet.
:- mode dec_unsigned_int(uo, mdi, muo) is semidet.
:- mode dec_unsigned_int(uo, in, out) is semidet.

:- pred percent_encode_path_chars : parser_pred.
:- mode percent_encode_path_chars(di, uo) is det.
:- mode percent_encode_path_chars(mdi, muo) is det.
:- mode percent_encode_path_chars(in, out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%----------------------------------------------------------------------------%

dec_digit(D) -->
    ( ['0'] -> { D = 0 }
    ; ['1'] -> { D = 1 }
    ; ['2'] -> { D = 2 }
    ; ['3'] -> { D = 3 }
    ; ['4'] -> { D = 4 }
    ; ['5'] -> { D = 5 }
    ; ['6'] -> { D = 6 }
    ; ['7'] -> { D = 7 }
    ; ['8'] -> { D = 8 }
    ; ['9'] -> { D = 9 }
    ;
        { fail }
    ).

dec_unsigned_int(I) -->
    dec_digit(D),
    dec_unsigned_int_loop(D, I).

:- pred dec_unsigned_int_loop : parser2_pred(int).
:- mode dec_unsigned_int_loop(di, uo, di, muo) is det.
:- mode dec_unsigned_int_loop(di, uo, mdi, muo) is det.
:- mode dec_unsigned_int_loop(di, uo, in, out) is det.

dec_unsigned_int_loop(I0, I) -->
    ( dec_digit(D) ->
        dec_unsigned_int_loop(I0 * 10 + D, I)
    ;
        { I = I0 + 0 }
    ).

percent_encode_path_chars([], []).

percent_encode_path_chars([Char | CharRest], EncodedChars) :-
    ( if
        ( Char = ('/') ; Char = ('\\') )
    then
        EncodedChars = ['/' | EncodedRest]
    else if
        ( Char = '0'; Char = '1'; Char = '2'; Char = '3'; Char = '4'
        ; Char = '5'; Char = '6'; Char = '7'; Char = '8'; Char = '9'
        ; Char = 'a'; Char = 'b'; Char = 'c'; Char = 'd'; Char = 'e'
        ; Char = 'f'; Char = 'g'; Char = 'h'; Char = 'i'; Char = 'j'
        ; Char = 'k'; Char = 'l'; Char = 'm'; Char = 'n'; Char = 'o'
        ; Char = 'p'; Char = 'q'; Char = 'r'; Char = 's'; Char = 't'
        ; Char = 'u'; Char = 'v'; Char = 'w'; Char = 'x'; Char = 'y'
        ; Char = 'z'
        ; Char = 'A'; Char = 'B'; Char = 'C'; Char = 'D'; Char = 'E'
        ; Char = 'F'; Char = 'G'; Char = 'H'; Char = 'I'; Char = 'J'
        ; Char = 'K'; Char = 'L'; Char = 'M'; Char = 'N'; Char = 'O'
        ; Char = 'P'; Char = 'Q'; Char = 'R'; Char = 'S'; Char = 'T'
        ; Char = 'U'; Char = 'V'; Char = 'W'; Char = 'X'; Char = 'Y'
        ; Char = 'Z'
        ; Char = '_'; Char = (':') ; Char = ('-') ; Char = ('.')
        )
    then
        char_copy(Char, Unique),
        EncodedChars = [Unique | EncodedRest]
    else
        ( if
            to_utf8(Char, Codes),
            (
                Codes = [C1],
                utf8_code_unit_to_hex(C1, C1H, C1L),
                EncodedChars0 = ['%', C1H, C1L | EncodedRest]
            ;
                Codes = [C1, C2],
                utf8_code_unit_to_hex(C1, C1H, C1L),
                utf8_code_unit_to_hex(C2, C2H, C2L),
                EncodedChars0 = ['%', C1H, C1L, '%', C2H, C2L | EncodedRest]
            ;
                Codes = [C1, C2, C3],
                utf8_code_unit_to_hex(C1, C1H, C1L),
                utf8_code_unit_to_hex(C2, C2H, C2L),
                utf8_code_unit_to_hex(C3, C3H, C3L),
                EncodedChars0 = ['%', C1H, C1L, '%', C2H, C2L, '%', C3H, C3L |
                    EncodedRest]
            ;
                Codes = [C1, C2, C3, C4],
                utf8_code_unit_to_hex(C1, C1H, C1L),
                utf8_code_unit_to_hex(C2, C2H, C2L),
                utf8_code_unit_to_hex(C3, C3H, C3L),
                utf8_code_unit_to_hex(C4, C4H, C4L),
                EncodedChars0 = ['%', C1H, C1L, '%', C2H, C2L, '%', C3H, C3L,
                    '%', C4H, C4L | EncodedRest]
            )
        then
            EncodedChars = EncodedChars0
        else
            unexpected($file, $pred, "Invalid UTF-8 code point")
        )
    ),
    percent_encode_path_chars(CharRest, EncodedRest).

:- pred utf8_code_unit_to_hex(int::in, char::uo, char::uo) is det.

utf8_code_unit_to_hex(Utf8Code::in, High::uo, Low::uo) :-
    char_copy(det_from_int(0'0 + (Utf8Code `unchecked_right_shift` 4)), High),
    char_copy(det_from_int(0'0 + (Utf8Code /\ 0xf)), Low).

:- pred char_copy(char, char).
:- mode char_copy(di, uo) is det.
:- mode char_copy(in, uo) is det.

:- pragma inline(char_copy/2).
:- pragma promise_equivalent_clauses(char_copy/2).

char_copy(Char::di, Char::uo).
char_copy(Char::in, Unique::uo) :-
    Unique = unsafe_promise_unique(det_from_int(to_int(Char) + 0)).

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.dcg_parsing.
%----------------------------------------------------------------------------%
