%%% @copyright 2012-2022, Michael Santos <michael.santos@gmail.com>

%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(stk500_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    stk500_program_load/1,
    counter/1
]).

-define(HEX_FILE, "counter.cpp.hex").

-define(INCR, 0).
-define(READ, 1).
-define(SET, 2).
-define(INT16(N), N:2 / little - integer - unit:8).

all() ->
    [{group, load}].

groups() ->
    [{load, [sequence], [stk500_program_load]}].

init_per_suite(Config) ->
    Serial = stk500:serial_device(),
    [{serial, Serial} | Config].

end_per_suite(Config) ->
    Config.

init_per_testcase(_Test, Config) ->
    Serial = ?config(serial, Config),
    {ok, FD} = stk500:open(Serial),
    [{fd, FD} | Config].

end_per_testcase(_Test, Config) ->
    FD = ?config(fd, Config),
    ok = serctl:close(FD),
    Config.

%% Upload code to an Arduino.
stk500_program_load(Config) ->
    FD = ?config(fd, Config),
    error_logger:info_report([{hex_file, codepath(?HEX_FILE)}]),
    Hex = stk500:hex_file(codepath(?HEX_FILE)),
    Bytes = stk500:chunk(Hex, 128),
    ok = stk500:load(FD, Bytes, [verbose]).

counter(Config) ->
    FD = ?config(fd, Config),
    Termios = lists:foldl(
        fun(Fun, Acc) -> Fun(Acc) end,
        serctl:mode(raw),
        [
            fun(N) -> serctl:flow(N, false) end,
            fun(N) -> serctl:ispeed(N, b9600) end,
            fun(N) -> serctl:ospeed(N, b9600) end
        ]
    ),
    ok = serctl:tcsetattr(FD, tcsanow, Termios),

    % Increment the counter
    ok = serctl:write(FD, <<?INT16(?INCR), ?INT16(1)>>),

    % Request the new value
    serctl:write(FD, <<?INT16(?READ), ?INT16(0)>>),
    {ok, <<2:2/integer-unit:8, ?INT16(1)>>} = serctl:readx(FD, 4).

codepath(File) ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        File
    ]).
