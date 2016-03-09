%% Copyright (c) 2012-2016, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
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
-define(INT16(N), N:2/little-integer-unit:8).

all() ->
    [{group, load}].

groups() ->
    [{load, [sequence], [stk500_program_load]}].

init_per_suite(Config) ->
    Serial = stk500:serial_device(),
    [{serial, Serial}|Config].

end_per_suite(Config) ->
    Config.

init_per_testcase(_Test, Config) ->
    Serial = ?config(serial, Config),
    {ok, FD} = stk500:open(Serial),
    [{fd, FD}|Config].

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
