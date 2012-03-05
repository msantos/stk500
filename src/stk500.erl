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
-module(stk500).
-include("serctl.hrl").
-include("stk500.hrl").

-export([
        open/0, open/1, open/2,

        reset/1,
        dtrrts/2,
        version/1,

        sync/1,
        read/1, read/2,

        hex_file/1, hex_file/2,
        chunk/2,
        load/2
    ]).

-define(DEV, "/dev/ttyUSB0").

-define(TIOCMGET, 16#5415).
-define(TIOCMSET, 16#5418).

-define(TIOCM_DTR, 16#002).
-define(TIOCM_RTS, 16#004).


open() ->
    open(?DEV, [{speed, b19200}]).

open(Dev) ->
    open(Dev, [{speed, b19200}]).

open(Dev, Opt) ->

    Speed = proplists:get_value(speed, Opt, b19200),

    % 19200, 8 data bits, 1 stop bit, no parity
    Mode = #termios{
        iflag = serctl:constant(ignbrk),
        oflag = 0,
        lflag = 0,
        cflag = serctl:constant(cs8)
        bor serctl:constant(clocal)
        bor serctl:constant(cread), 

        cc = lists:foldl(
            fun({Offset, Val}, Bin) ->
                    serctl:offset(Bin, {Offset, Val})
            end,
            <<0:(serctl:constant(nccs)*8)>>,   % zero'ed bytes
            [
                {serctl:constant(vmin), 1},    % Minimum number of characters
                {serctl:constant(vtime), 0}    % Timeout in deciseconds
            ])
    },

    Termios = lists:foldl(
        fun(Fun, Acc) -> Fun(Acc) end,
        Mode,
        [
            fun(N) -> serctl:ispeed(N, Speed) end,
            fun(N) -> serctl:ospeed(N, Speed) end,

            fun(N) -> serctl:flow(N, false) end
        ]),

    {ok, FD} = serctl:open(Dev),
    ok = serctl:tcsetattr(FD, tcsanow, Termios),

    {ok, FD}.


reset(FD) ->
    ok = dtrrts(FD, off),
    timer:sleep(50),
    ok = dtrrts(FD, on),
    timer:sleep(50),

    [ begin read(FD), sync(FD) end
        || _ <- lists:seq(1,3) ],

    read(FD),

    ok.


version(FD) ->
    reset(FD),

    ok = serctl:write(FD, <<?Cmnd_STK_GET_PARAMETER, ?Parm_STK_SW_MAJOR, ?Sync_CRC_EOP>>),
    {ok, <<?Resp_STK_INSYNC, Major, ?Resp_STK_OK>>} = read(FD, 3),

    ok = serctl:write(FD, <<?Cmnd_STK_GET_PARAMETER, ?Parm_STK_SW_MINOR, ?Sync_CRC_EOP>>),
    {ok, <<?Resp_STK_INSYNC, Minor, ?Resp_STK_OK>>} = read(FD, 3),

    {Major, Minor}.


dtrrts(FD, Status) when is_integer(Status) ->
    dtrrts(FD, bool(Status));
dtrrts(FD, Status) when is_atom(Status) ->
    {ok, <<Ctl:4/native-unsigned-integer-unit:8>>} = serctl:ioctl(
        FD,
        ?TIOCMGET,
        <<0:32>>
    ),

    Ctl1 = case Status of
        on -> Ctl bor ( ?TIOCM_DTR bor ?TIOCM_RTS );
        off -> Ctl band bnot ( ?TIOCM_DTR bor ?TIOCM_RTS )
    end,

    {ok, <<_Ctl2:4/native-unsigned-integer-unit:8>>} = serctl:ioctl(
        FD,
        ?TIOCMSET,
        <<Ctl1:4/native-unsigned-integer-unit:8>>
    ),

    ok.


% <<16#30, 16#20>>
sync(FD) ->
    serctl:write(FD, <<?Cmnd_STK_GET_SYNC, ?Sync_CRC_EOP>>).


read(FD) ->
    read(FD, 2).
read(FD, N) ->
    poll(FD, N, 10).


hex_file(File) ->
    hex_file(intel, File).

hex_file(intel, File) ->
    {ok, Bin} = file:read_file(File),
    Hex = binary:split(Bin, <<"\r\n">>, [global, trim]),
    rec(Hex).


load(FD, Bytes) ->
    reset(FD),

    ok = serctl:write(FD, <<?Cmnd_STK_ENTER_PROGMODE, ?Sync_CRC_EOP>>),
    case read(FD, 2) of
        {ok, <<?Resp_STK_INSYNC, ?Resp_STK_OK>>} ->
            load_1(FD, 0, Bytes);
        {ok, Resp} ->
            {protocol_error, {response, Resp}};
        {error, Error} ->
            {error, enter_progmode, Error}
    end.

load_1(FD, _Address, []) ->
    ok = serctl:write(FD, <<
        ?Cmnd_STK_LEAVE_PROGMODE,
        ?Sync_CRC_EOP
        >>),

    case read(FD, 2) of
        {ok, <<?Resp_STK_INSYNC, ?Resp_STK_OK>>} ->
            ok;
        {ok, Resp} ->
            {protocol_error, {response, Resp}};
        {error, Error} ->
            {error, leave_progmode, Error}
    end;

load_1(FD, Address, Bytes) ->
    Cmd = <<?Cmnd_STK_LOAD_ADDRESS,
        Address:2/little-unsigned-integer-unit:8,
        ?Sync_CRC_EOP
        >>,

    ok = serctl:write(FD, Cmd),

    error_logger:info_report([
            {address, Address},
            {cmd, Cmd}
        ]),

    case read(FD, 2) of
        {ok, <<?Resp_STK_INSYNC, ?Resp_STK_OK>>} ->
            load_2(FD, Address, Bytes);
        {ok, Resp} ->
            {protocol_error, {response, Resp}};
        {error, Error} ->
            {error, {load_address, Address}, Error}
    end.

load_2(FD, Address, [H|T]) ->
    Cmd = <<?Cmnd_STK_PROG_PAGE,
        (byte_size(H)):2/big-unsigned-integer-unit:8,
        $F,
        H/bytes,
        ?Sync_CRC_EOP>>,

    error_logger:info_report([
            {address, Address},
            {cmd, Cmd},
            {data, H}
        ]),

    ok = serctl:write(FD, Cmd),

    case read(FD, 2) of
        {ok, <<?Resp_STK_INSYNC, ?Resp_STK_OK>>} ->
            load_1(FD, Address+byte_size(H) div 2, T);
        {ok, <<?Resp_STK_INSYNC>>} ->
            read(FD,1),
            load_1(FD, Address+byte_size(H) div 2, T);
        {ok, Resp} ->
            {protocol_error, {response, Resp}};
        {error, Error} ->
            {error, {prog_page, Address, H}, Error}
    end.


chunk(Bytes, Size) when is_list(Bytes) ->
    chunk(list_to_binary(Bytes), Size);
chunk(Bytes, Size) when is_binary(Bytes) ->
    chunk(Bytes, Size, []).

chunk(Bytes, Size, Acc) ->
    case Bytes of
        <<Chunk:Size/bytes, Rest/binary>> ->
            chunk(Rest, Size, [Chunk|Acc]);
        _ ->
            lists:reverse([Bytes|Acc])
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
bin_to_int(N, Base) ->
    list_to_integer(binary_to_list(N), Base).


poll(_FD, _Num, 0) ->
    {error, eagain};
poll(FD, NumBytes, Retry) ->
    case serctl:read(FD, NumBytes) of
        {error, eagain} ->
            timer:sleep(25),
            poll(FD, NumBytes, Retry-1);
        {ok, Buf} ->
            {ok, Buf};
        Error ->
            Error
    end.


bool(0) -> off;
bool(1) -> on.


rec(Hex) ->
    rec(Hex, []).

rec([], Acc) ->
    lists:reverse(Acc);

% Type 0
% XXX Should compute and verify checksum
rec([<<":", LenX:2/bytes, _LoadX:4/bytes, "00", DataSum/binary>>|T], Acc) ->
    Len = bin_to_int(LenX, 16),

    % Each byte is encoded as 2 bytes hex
    HexLen = Len * 2,
    <<Data:HexLen/bytes, _Sum:2/bytes>> = DataSum,

    Bytes = rec_data(Data),

    rec(T, [list_to_binary(Bytes)|Acc]);

% Ignore other types
rec([_|T], Acc) ->
    rec(T, Acc).


rec_data(Data) ->
    rec_data(Data, []).

rec_data(<<>>, Acc) ->
    lists:reverse(Acc);
rec_data(<<Byte:2/bytes, Rest/binary>>, Acc) ->
    rec_data(Rest, [bin_to_int(Byte, 16)|Acc]).
