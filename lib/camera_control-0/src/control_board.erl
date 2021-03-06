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

-module (control_board).

-define (REGNAME, ?MODULE).

-behaviour (gen_server).

%% API
-export ([start_link/1, read_input/0, select_video/1]).

%% gen_server callbacks
-export ([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3]).

%% State
-record (state, {port}).


%% API

start_link (ParallelPort) ->
  gen_server:start_link ({local, ?REGNAME}, ?MODULE, ParallelPort, []).

read_input () ->
  gen_server:call (?REGNAME, read_input).

select_video (Num) ->
  gen_server:call (?REGNAME, {select_video, Num}).


%% gen_server callbacks

init (ParallelPort) ->
  process_flag (trap_exit, true),

  Port = port:open (code:priv_dir (camera_control) ++ "/parallel_port_drv",
                    [ParallelPort]),

  {ok, #state{port=Port}}.

terminate ({port_terminated, _Reason}, _State) ->
  ok;

terminate (_Reason, #state{port=Port} = _State) ->
  port:close (Port),
  ok.


handle_call (read_input, _From, State) ->
  {ok, Result} = read_input (State),
  {reply, {ok, Result}, State};

handle_call ({select_video, Num}, _From, State) ->
  ok = select_video (Num, State),
  {reply, ok, State}.


handle_cast (_Request, State) ->
  {noreply, State}.

handle_info ({'EXIT', _Port, Reason}, State) ->
  {stop, {port_terminated, Reason}, State};

handle_info (_Info, State) ->
  {noreply, State}.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.



%% Input scanning

% Note that the hardware reports invalid data if multiple buttons are pressed
% simultaneously.
read_input (State) ->
  <<XBits:16/bits, YBits:16/bits>> = read_raw_input (State),
  %<<XN:16>> = XBits, <<YN:16>> = YBits,
  %error_logger:info_msg ("Got bits: Y ~16.2.0B X ~16.2.0B~n", [YN, XN]),
  {ok, match_axis (YBits, XBits)}.

read_raw_input (State) ->
  read_raw_input (0, <<>>, State).

read_raw_input (N, Bits, #state{port=Port} = State) when N < 8 ->
  ok = port:send (Port, {write_data, <<N, 2#1111>>}),
  ok = timer:sleep (1),

  {ok, <<StatusInv>>} = port:send (Port, read_status),
  Status = bnot StatusInv,

  <<NewBits:4, _:4>> = <<Status>>,

  read_raw_input (N+1, <<NewBits:4, Bits/bits>>, State);

read_raw_input (_N, <<_:32>> = Bits, _State) ->
  Bits.

match_axis (YBits, XBits) ->
  [{Y, X} || Y <- lists:seq (0, 15),
             nth_bit (Y, YBits) =/= 0,
             X <- lists:seq (0, 15),
             nth_bit (X, XBits) =/= 0].

nth_bit (N, Bits) ->
  NBefore = bit_size (Bits)-1-N,
  <<_:NBefore, Bit:1, _:N>> = Bits,
  Bit.


%% Video selection

select_video (Num, #state{port=Port} = _State) ->
  error_logger:info_msg ("Select video ~p", [Num]),

  ok = port:send (Port, {write_control, <<0, 2#1>>}),
  ok = port:send (Port, {write_data, <<0, 2#11110000>>}),

  <<Control0:1, Data7654:4>> = <<(1 bsl Num):5>>,

  ok = port:send (Port, {write_control, <<0:7, Control0:1, 2#1>>}),
  ok = port:send (Port, {write_data, <<Data7654:4, 0:4, 2#11110000>>}),

  ok.

% vim:set et sw=2 sts=2:
