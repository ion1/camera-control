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
  YBits = read_raw_input (0, State),
  XBits = read_raw_input (4, State),

  Table = match_axis (YBits, XBits),
  {ok, pressed_buttons (Table)}.

read_raw_input (Offset, State) ->
  read_raw_input (Offset, 0, <<>>, State).

read_raw_input (Offset, N, Bits, #state{port=Port} = State) when N < 4 ->
  ok = port:send (Port, {write_data, <<(Offset+N), 2#1111>>}),
  ok = timer:sleep (1),

  {ok, <<StatusInv>>} = port:send (Port, read_status),
  Status = bnot StatusInv,

  <<NewBits:4, _:4>> = <<Status>>,

  read_raw_input (Offset, N+1, <<NewBits:4, Bits/bits>>, State);

read_raw_input (_Offset, 4, Bits, _State) ->
  Bits.

match_axis (YBits, XBits) ->
  lists:map  (fun (Y) ->
      YActive = nth_bit (Y, YBits) =/= 0,
      lists:map  (fun (X) ->
          XActive = nth_bit (X, XBits) =/= 0,
          YActive andalso XActive end,
        lists:seq (0, 15)) end,
    lists:seq (0, 15)).

nth_bit (N, Bits) ->
  NBefore = bit_size (Bits)-1-N,
  <<_:NBefore, Bit:1, _:N>> = Bits,
  Bit.

pressed_buttons ([Row|RowsTail]) ->
  pressed_buttons (RowsTail, Row, 0, 0, []).

pressed_buttons (RowsTail, [Col|ColsTail], Y, X, Result) ->
  NewResult = if
    Col ->
      Result ++ [{Y, X}];
    true ->
      Result end,
  pressed_buttons (RowsTail, ColsTail, Y, X+1, NewResult);

pressed_buttons ([Row|RowsTail], [], Y, _X, Result) ->
  pressed_buttons (RowsTail, Row, Y+1, 0, Result);

pressed_buttons ([], [], _Y, _X, Result) ->
  Result.


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
