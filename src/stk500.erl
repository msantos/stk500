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
-module(stk500).
-include_lib("srly/include/serctl.hrl").
-include("stk500.hrl").

-export([
    open/0, open/1, open/2,

    reset/1,
    dtrrts/2,
    version/1,

    sync/1,

    hex_file/1, hex_file/2,
    chunk/2,
    load/2, load/3,
    cmd/2, cmd/3,

    serial_device/0
]).

-define(DEV, "/dev/ttyUSB0").

-define(TIOCMGET, serctl:constant(tiocmget)).
-define(TIOCMSET, serctl:constant(tiocmset)).

-define(TIOCM_DTR, serctl:constant(tiocm_dtr)).
-define(TIOCM_RTS, serctl:constant(tiocm_rts)).

-define(STK500_OK, <<?Resp_STK_INSYNC, ?Resp_STK_OK>>).

%% @doc Connect to the Arduino using the default serial device at
%% 19200 baud
-spec open() -> {ok, serctl:fd()} | {error, file:posix()}.
open() ->
    Dev = serial_device(),
    open(Dev, [{speed, b19200}]).

%% @doc Connect to the Arduino using the specified serial device at
%% 19200 baud
-spec open(serctl:dev()) -> {ok, serctl:fd()} | {error, file:posix()}.
open(Dev) ->
    open(Dev, [{speed, b19200}]).

%% @doc Connect to the Arduino using the specified serial device and settings
%%
%% ```
%% % The Diecimila uses 19200
%% {ok,FD} = stk500:open("/dev/ttyUSB0", [{speed, 19200}]).
%% '''
-spec open(serctl:dev(), proplists:proplist()) -> {ok, serctl:fd()} | {error, file:posix()}.
open(Dev, Opt) ->
    Speed = proplists:get_value(speed, Opt, b19200),

    % 19200, 8 data bits, 1 stop bit, no parity
    Mode = #termios{
        iflag = serctl:constant(ignbrk),
        oflag = 0,
        lflag = 0,
        cflag =
            serctl:constant(cs8) bor
                serctl:constant(clocal) bor
                serctl:constant(cread),

        cc = lists:foldl(
            fun({Offset, Val}, Bin) ->
                serctl:offset(Bin, {Offset, Val})
            end,
            % zero'ed bytes
            <<0:(serctl:constant(nccs) * 8)>>,
            [
                % Minimum number of characters
                {serctl:constant(vmin), 1},
                % Timeout in deciseconds
                {serctl:constant(vtime), 0}
            ]
        )
    },

    Termios = lists:foldl(
        fun(Fun, Acc) -> Fun(Acc) end,
        Mode,
        [
            fun(N) -> serctl:ispeed(N, Speed) end,
            fun(N) -> serctl:ospeed(N, Speed) end,

            fun(N) -> serctl:flow(N, false) end
        ]
    ),

    case serctl:open(Dev) of
        {ok, FD} ->
            case serctl:tcsetattr(FD, tcsanow, Termios) of
                ok ->
                    {ok, FD};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Reset the Arduino
-spec reset(serctl:fd()) -> ok.
reset(FD) ->
    [
        begin
            dtrrts(FD, Status),
            timer:sleep(50)
        end
     || Status <- [off, on]
    ],

    [
        begin
            serctl:readx(FD, 2, 250),
            sync(FD)
        end
     || _ <- lists:seq(1, 3)
    ],

    serctl:readx(FD, 2, 250),

    ok.

%% @doc Get version
-spec version(serctl:fd()) -> {byte(), byte()}.
version(FD) ->
    reset(FD),

    Size = [{size, 3}],

    {ok, <<?Resp_STK_INSYNC, Major, ?Resp_STK_OK>>} = cmd(
        FD,
        <<?Cmnd_STK_GET_PARAMETER, ?Parm_STK_SW_MAJOR, ?Sync_CRC_EOP>>,
        Size
    ),

    {ok, <<?Resp_STK_INSYNC, Minor, ?Resp_STK_OK>>} = cmd(
        FD,
        <<?Cmnd_STK_GET_PARAMETER, ?Parm_STK_SW_MINOR, ?Sync_CRC_EOP>>,
        Size
    ),

    {Major, Minor}.

-spec dtrrts(serctl:fd(), 0 | 1 | boolean()) -> ok.
dtrrts(FD, Status) when is_integer(Status) ->
    dtrrts(FD, bool(Status));
dtrrts(FD, Status) when is_atom(Status) ->
    {ok, <<Ctl:4/native-unsigned-integer-unit:8>>} = serctl:ioctl(
        FD,
        ?TIOCMGET,
        <<0:32>>
    ),

    Ctl1 =
        case Status of
            on -> Ctl bor (?TIOCM_DTR bor ?TIOCM_RTS);
            off -> Ctl band bnot (?TIOCM_DTR bor ?TIOCM_RTS)
        end,

    {ok, <<_Ctl2:4/native-unsigned-integer-unit:8>>} = serctl:ioctl(
        FD,
        ?TIOCMSET,
        <<Ctl1:4/native-unsigned-integer-unit:8>>
    ),

    ok.

% <<16#30, 16#20>>
-spec sync(serctl:fd()) -> ok | {error, file:posix()}.
sync(FD) ->
    cmd(FD, <<?Cmnd_STK_GET_SYNC, ?Sync_CRC_EOP>>).

%% @doc Read a hex file generated by the Arduino IDE
%%
%% hex_file/1 returns a list of binaries as read from the
%% % file (16 bytes). It's faster to use 128 bytes chunks.
-spec hex_file(file:name_all()) -> [byte()].
hex_file(File) ->
    hex_file(intel, File).

-spec hex_file(intel, file:name_all()) -> ok.
hex_file(intel, File) ->
    {ok, Bin0} = file:read_file(File),
    Bin = binary:replace(Bin0, <<"\r\n">>, <<"\n">>, [global]),
    Hex = binary:split(Bin, <<"\n">>, [global, trim]),
    rec(Hex).

%% @doc Load the hex file
-spec load(serctl:fd(), [byte()]) -> ok.
load(FD, Bytes) ->
    load(FD, Bytes, []).

-spec load(serctl:fd(), [byte()], proplists:proplist()) -> ok.
load(FD, Bytes, Opt) when is_list(Opt) ->
    reset(FD),

    {ok, ?STK500_OK} = cmd(FD, <<?Cmnd_STK_ENTER_PROGMODE, ?Sync_CRC_EOP>>, Opt),

    lists:foldl(
        fun(Buf, Address) ->
            {ok, ?STK500_OK} = cmd(
                FD,
                <<?Cmnd_STK_LOAD_ADDRESS, Address:2/little-unsigned-integer-unit:8, ?Sync_CRC_EOP>>,
                Opt
            ),

            {ok, ?STK500_OK} = cmd(
                FD,
                <<?Cmnd_STK_PROG_PAGE, (byte_size(Buf)):2/big-unsigned-integer-unit:8, $F,
                    Buf/bytes, ?Sync_CRC_EOP>>,
                Opt
            ),
            Address + byte_size(Buf) div 2
        end,
        0,
        Bytes
    ),

    {ok, ?STK500_OK} = cmd(FD, <<?Cmnd_STK_LEAVE_PROGMODE, ?Sync_CRC_EOP>>, Opt),

    ok.

cmd(FD, Cmd) ->
    cmd(FD, Cmd, []).

cmd(FD, Cmd, Opt) ->
    Verbose = proplists:get_value(verbose, Opt, false),

    verbose(Verbose, [{cmd, Cmd}]),

    case serctl:write(FD, Cmd) of
        ok ->
            cmd_1(FD, Cmd, Opt);
        {error, Error} ->
            {error, Error}
    end.

cmd_1(FD, _Cmd, Opt) ->
    Size = proplists:get_value(size, Opt, 2),

    case serctl:readx(FD, Size) of
        {ok, Resp} ->
            {ok, Resp};
        {error, Error} ->
            {error, Error}
    end.

chunk(Bytes, Size) when is_list(Bytes) ->
    chunk(list_to_binary(Bytes), Size);
chunk(Bytes, Size) when is_binary(Bytes) ->
    chunk(Bytes, Size, []).

chunk(Bytes, Size, Acc) when Size > 0, Size rem 2 == 0, Size =< 256 ->
    case Bytes of
        <<Chunk:Size/bytes, Rest/binary>> ->
            chunk(Rest, Size, [Chunk | Acc]);
        _ ->
            lists:reverse([Bytes | Acc])
    end.

%% @doc Try to discover the serial device
%%
%% Lookup the serial device in an environment variable: STK500_SERIAL_PORT
%%
%% Defaults to: /dev/ttyUSB0
serial_device() ->
    case os:getenv("STK500_SERIAL_PORT") of
        false ->
            ?DEV;
        Serial ->
            Serial
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
bin_to_int(N, Base) ->
    list_to_integer(binary_to_list(N), Base).

bool(0) -> off;
bool(1) -> on.

rec(Hex) ->
    rec(Hex, []).

rec([], Acc) ->
    lists:reverse(Acc);
% Type 0
% XXX Should compute and verify checksum
rec([<<":", LenX:2/bytes, _LoadX:4/bytes, "00", DataSum/binary>> | T], Acc) ->
    Len = bin_to_int(LenX, 16),

    % Each byte is encoded as 2 bytes hex
    HexLen = Len * 2,
    <<Data:HexLen/bytes, _Sum:2/bytes>> = DataSum,

    Bytes = rec_data(Data),

    rec(T, [list_to_binary(Bytes) | Acc]);
% Ignore other types
rec([_ | T], Acc) ->
    rec(T, Acc).

rec_data(Data) ->
    rec_data(Data, []).

rec_data(<<>>, Acc) ->
    lists:reverse(Acc);
rec_data(<<Byte:2/bytes, Rest/binary>>, Acc) ->
    rec_data(Rest, [bin_to_int(Byte, 16) | Acc]).

verbose(false, _) ->
    ok;
verbose(true, Data) ->
    error_logger:info_report(Data).
