% camera-control – Control Sanyo PTZ cameras with a custom input board
%
% Copyright © 2009 Johan Kiviniemi
%
% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module (ssp).

-define (REGNAME, ?MODULE).

-behaviour (gen_server).

%% API
-export ([start_link/1, send/3]).

%% gen_server callbacks
-export ([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3]).

%% includes
-include ("ssp_constants.hrl").

%% State
-record (state, {port,
                 my_addr,
                 sent_addr_cat = 0,
                 sent_addr_dev = 0,
                 sent_time     = 0,
                 sleep_until   = 0}).


%% API

start_link (SerialPort, MyAddr) ->
  gen_server:start_link ({local, ?REGNAME}, ?MODULE, {SerialPort, MyAddr}, []).

start_link (SerialPort) ->
  start_link (SerialPort, 0).

send (AddrCat, AddrDev, Data) ->
  gen_server:call (?REGNAME, {send, AddrCat, AddrDev, Data}).

%% gen_server callbacks

init ({SerialPort, MyAddr}) ->
  process_flag (trap_exit, true),

  Port = port:open (code:priv_dir (camera_control) ++ "/serial_port_drv",
                    [SerialPort]),

  {ok, #state{port=Port, my_addr=MyAddr}}.

terminate ({port_terminated, _Reason}, _State) ->
  ok;

terminate (_Reason, #state{port=Port} = _State) ->
  port:close (Port),
  ok.


handle_call ({send, _AddrCat, _AddrDev, []}, _From, State) ->
  {reply, ok, State};

handle_call ({send, AddrCat, AddrDev, DataPayload}, _From, State) ->
  Now = now_ms (),

  % Commands are not sent without handshake if this delay has passed.
  BusFreeAt = State#state.sent_time + ?SSP_DELAY_BUS_FREE,
  % Delay a bit more before the handshake to make sure the bus is free.
  BusFreeForSureAt = State#state.sent_time + ?SSP_DELAY_RELEASE_BUS,

  FirstCmd = State#state.sent_time == 0,
  BusFree  = BusFreeAt =< Now,
  SameAddr = (AddrCat == State#state.sent_addr_cat) andalso
             (AddrDev == State#state.sent_addr_dev),

  NeedHandshake = if
    FirstCmd orelse BusFree orelse (not SameAddr) ->
      true;
    true ->
      false end,

  DataInitial = if
    NeedHandshake andalso (not FirstCmd) ->
      [{sleep_until, BusFreeForSureAt}];
    true ->
      [] end,

  DataHandshake = if
    NeedHandshake ->
      [<<?SSP_ADDR_CAT_CONTROLLER, (State#state.my_addr),
         AddrCat, AddrDev,
         ?SSP_TRANSMISSION_START>>,
       delay];
    true ->
      [] end,

  Data = DataInitial ++ DataHandshake ++ DataPayload,

  error_logger:info_msg ("SSP: Send: ~p", [Data]),

  send_data (Data, State#state{sent_addr_cat=AddrCat, sent_addr_dev=AddrDev}).


handle_cast (_Request, State) ->
  {noreply, State}.

handle_info (_Info, State) ->
  {noreply, State}.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.


%% private

send_data ([{sleep_until, Time} | Tail], State) ->
  SleepUntil = erlang:max (State#state.sleep_until, Time),
  send_data (Tail, State#state{sleep_until=SleepUntil});

send_data ([delay | Tail], State) ->
  SleepUntil = State#state.sleep_until + ?SSP_DELAY_BYTES,
  send_data (Tail, State#state{sleep_until=SleepUntil});

send_data ([<<Byte, ByteTail/bytes>> | Tail], #state{port=Port} = State) ->
  ok = sleep_until (State#state.sleep_until),

  port:send (Port, {write, <<Byte>>}),

  SentTime   = now_ms (),
  SleepUntil = SentTime + ?SSP_DELAY_BYTES,

  send_data ([ByteTail | Tail],
             State#state{sent_time=SentTime, sleep_until=SleepUntil});

send_data ([<<>> | Tail], State) ->
  send_data (Tail, State);

send_data ([], State) ->
  {reply, ok, State}.

now_ms () ->
  {MegaS, S, MicroS} = now (),
  1000*1000*1000*MegaS + 1000*S + trunc (0.001*MicroS).

sleep_until (Time) ->
  MilliS = Time - now_ms (),
  if
    MilliS > 0 ->
      ok = timer:sleep (MilliS);
    true ->
      ok end.

% vim:set et sw=2 sts=2:
