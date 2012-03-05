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

% %**** ATMEL AVR - A P P L I C A T I O N   N O T E  ************************
% %*
% %* Title:		AVR061 - STK500 Communication Protocol
% %* Filename:		command.h
% %* Version:		1.0
% %* Last updated:	09.09.2002
% %*
% %* Support E-mail:	avr@atmel.com
% %*
% %**************************************************************************

% % *****************[ STK Message constants ]***************************

-define(STK_SIGN_ON_MESSAGE, "AVR STK" ).  % Sign on string for Cmnd_STK_GET_SIGN_ON

% % *****************[ STK Response constants ]***************************

-define(Resp_STK_OK              ,  16#10  ).
-define(Resp_STK_FAILED          ,  16#11  ).
-define(Resp_STK_UNKNOWN         ,  16#12  ).
-define(Resp_STK_NODEVICE        ,  16#13  ).
-define(Resp_STK_INSYNC          ,  16#14  ).
-define(Resp_STK_NOSYNC          ,  16#15  ).

-define(Resp_ADC_CHANNEL_ERROR   ,  16#16  ).
-define(Resp_ADC_MEASURE_OK      ,  16#17  ).
-define(Resp_PWM_CHANNEL_ERROR   ,  16#18  ).
-define(Resp_PWM_ADJUST_OK       ,  16#19  ).

% % *****************[ STK Special constants ]***************************

-define(Sync_CRC_EOP             ,  16#20  ). % 'SPACE'

% *****************[ STK Command constants ]***************************

-define(Cmnd_STK_GET_SYNC        ,  16#30  ).
-define(Cmnd_STK_GET_SIGN_ON     ,  16#31  ).

-define(Cmnd_STK_SET_PARAMETER   ,  16#40  ).
-define(Cmnd_STK_GET_PARAMETER   ,  16#41  ).
-define(Cmnd_STK_SET_DEVICE      ,  16#42  ).
-define(Cmnd_STK_SET_DEVICE_EXT  ,  16#45  ).			

-define(Cmnd_STK_ENTER_PROGMODE  ,  16#50  ).
-define(Cmnd_STK_LEAVE_PROGMODE  ,  16#51  ).
-define(Cmnd_STK_CHIP_ERASE      ,  16#52  ).
-define(Cmnd_STK_CHECK_AUTOINC   ,  16#53  ).
-define(Cmnd_STK_LOAD_ADDRESS    ,  16#55  ).
-define(Cmnd_STK_UNIVERSAL       ,  16#56  ).
-define(Cmnd_STK_UNIVERSAL_MULTI ,  16#57  ).

-define(Cmnd_STK_PROG_FLASH      ,  16#60  ).
-define(Cmnd_STK_PROG_DATA       ,  16#61  ).
-define(Cmnd_STK_PROG_FUSE       ,  16#62  ).
-define(Cmnd_STK_PROG_LOCK       ,  16#63  ).
-define(Cmnd_STK_PROG_PAGE       ,  16#64  ).
-define(Cmnd_STK_PROG_FUSE_EXT   ,  16#65  ).		

-define(Cmnd_STK_READ_FLASH      ,  16#70  ).
-define(Cmnd_STK_READ_DATA       ,  16#71  ).
-define(Cmnd_STK_READ_FUSE       ,  16#72  ).
-define(Cmnd_STK_READ_LOCK       ,  16#73  ).
-define(Cmnd_STK_READ_PAGE       ,  16#74  ).
-define(Cmnd_STK_READ_SIGN       ,  16#75  ).
-define(Cmnd_STK_READ_OSCCAL     ,  16#76  ).
-define(Cmnd_STK_READ_FUSE_EXT   ,  16#77  ).		
-define(Cmnd_STK_READ_OSCCAL_EXT ,  16#78  ).     

% *****************[ STK Parameter constants ]***************************

-define(Parm_STK_HW_VER         ,   16#80  ). % R
-define(Parm_STK_SW_MAJOR       ,   16#81  ). % R
-define(Parm_STK_SW_MINOR       ,   16#82  ). % R
-define(Parm_STK_LEDS           ,   16#83  ). % R/W
-define(Parm_STK_VTARGET        ,   16#84  ). % R/W
-define(Parm_STK_VADJUST        ,   16#85  ). % R/W
-define(Parm_STK_OSC_PSCALE     ,   16#86  ). % R/W
-define(Parm_STK_OSC_CMATCH     ,   16#87  ). % R/W
-define(Parm_STK_RESET_DURATION ,   16#88  ). % R/W
-define(Parm_STK_SCK_DURATION   ,   16#89  ). % R/W

-define(Parm_STK_BUFSIZEL       ,   16#90  ). % R/W, Range {0..255}
-define(Parm_STK_BUFSIZEH       ,   16#91  ). % R/W, Range {0..255}
-define(Parm_STK_DEVICE         ,   16#92  ). % R/W, Range {0..255}
-define(Parm_STK_PROGMODE       ,   16#93  ). % 'P' or 'S'
-define(Parm_STK_PARAMODE       ,   16#94  ). % TRUE or FALSE
-define(Parm_STK_POLLING        ,   16#95  ). % TRUE or FALSE
-define(Parm_STK_SELFTIMED      ,   16#96  ). % TRUE or FALSE


% *****************[ STK status bit definitions ]***************************

-define(Stat_STK_INSYNC         ,   16#01  ). % INSYNC status bit, '1' - INSYNC
-define(Stat_STK_PROGMODE       ,   16#02  ). % Programming mode,  '1' - PROGMODE
-define(Stat_STK_STANDALONE     ,   16#04  ). % Standalone mode,   '1' - SM mode
-define(Stat_STK_RESET          ,   16#08  ). % RESET button,      '1' - Pushed
-define(Stat_STK_PROGRAM        ,   16#10  ). % Program button, '   1' - Pushed
-define(Stat_STK_LEDG           ,   16#20  ). % Green LED status,  '1' - Lit
-define(Stat_STK_LEDR           ,   16#40  ). % Red LED status,    '1' - Lit
-define(Stat_STK_LEDBLINK       ,   16#80  ). % LED blink ON/OFF,  '1' - Blink
