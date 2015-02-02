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

:- type parser_pred(T) == pred(T, chars, chars).

:- type parser2_pred(T) == pred(T, T, chars, chars).

%----------------------------------------------------------------------------%

:- pred dec_digit : parser_pred(int).
:- mode dec_digit(uo, mdi, muo) is semidet.
:- mode dec_digit(uo, di, uo) is semidet.
:- mode dec_digit(uo, in, out) is semidet.

:- pred dec_unsigned_int : parser_pred(int).
:- mode dec_unsigned_int(uo, di, muo) is semidet.
:- mode dec_unsigned_int(uo, mdi, muo) is semidet.
:- mode dec_unsigned_int(uo, in, out) is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

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

%----------------------------------------------------------------------------%
:- end_module mercury_mpm.dcg_parsing.
%----------------------------------------------------------------------------%
