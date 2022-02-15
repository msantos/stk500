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

-define(INT16(N), N:2 / little - integer - unit:8).

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

    {ok, FD} = serctl:open(Dev),

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

    {ok, FD}.

incr(FD) ->
    incr(FD, 1).

incr(FD, N) when is_integer(N) ->
    serctl:write(FD, <<?INT16(?INCR), ?INT16(N)>>).

read(FD) ->
    serctl:write(FD, <<?INT16(?READ), ?INT16(0)>>),
    {ok, <<2:2/integer-unit:8, N:2/little-integer-unit:8>>} = serctl:readx(FD, 4),
    {ok, N}.

set(FD, N) when is_integer(N) ->
    serctl:write(FD, <<?INT16(?SET), ?INT16(N)>>).
