%% Copyright (c) 2012, Michael Santos <michael.santos@gmail.com>
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
-module(counter).
-export([
        run/0,
        start/0, start/1,

        incr/1, incr/2,
        read/1,
        set/2
    ]).

-define(DEV, "/dev/ttyUSB0").

-define(INCR, 0).
-define(READ, 1).
-define(SET, 2).

-define(INT16(N), N:2/little-integer-unit:8).

run() ->
    {ok, FD} = start(),
    run_1(FD).

run_1(FD) ->
    ok = incr(FD),
    run_2(FD).

run_2(FD) ->
    case read(FD) of
        {ok, N} ->
            error_logger:info_report([{count, N}]),
            run_1(FD);
        {error, eagain} ->
            run_2(FD)
    end.


start() ->
    start([]).

start(Opt) ->
    Dev = proplists:get_value(dev, Opt, ?DEV),

    {ok,FD} = serctl:open(Dev),

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

   {ok,FD}.


incr(FD) ->
    incr(FD,1).

incr(FD, N) when is_integer(N) ->
    serctl:write(FD, <<?INT16(?INCR), ?INT16(N)>>).

read(FD) ->
    serctl:write(FD, <<?INT16(?READ), ?INT16(0)>>),
    {ok, <<2:2/integer-unit:8, N:2/little-integer-unit:8>>} = serctl:readx(FD, 4),
    {ok, N}.

set(FD, N) when is_integer(N) ->
    serctl:write(FD, <<?INT16(?SET), ?INT16(N)>>).
