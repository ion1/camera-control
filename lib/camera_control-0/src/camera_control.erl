-module (camera_control).

-define (REGNAME, ?MODULE).

-define (SAVE_TIMEOUT, 1000).

-behaviour (gen_fsm).

%% API
-export ([start_link/0, pressed/1, released/1]).

%% gen_fsm callbacks
-export ([init/1, terminate/3, idle/2, ptzing/2, saving_0/2, saving_1/2,
          handle_event/3, handle_sync_event/4, handle_info/3, code_change/4]).

%% State
-record (state, {active_channel=false,
                 last_button=none,
                 saving_id=none}).


%% API

start_link () ->
  gen_fsm:start_link ({local, ?REGNAME}, ?MODULE, dummy, []).

pressed (Button) ->
  gen_fsm:send_event (?REGNAME, {pressed, Button}).

released (Button) ->
  gen_fsm:send_event (?REGNAME, {released, Button}).


%% gen_fsm callbacks

init (dummy) ->
  process_flag (trap_exit, true),

  InitialState = #state{},

  % Load idle slot.
  State = case database:action_lookup ("idle") of
    {ok, {load, Channel, Slot}} ->
      {ok, NewState} = activate_slot (Channel, Slot, InitialState),
      NewState;

    _ ->
      error_logger:info_msg ("No idle slot; activating channel 0"),
      {ok, NewState} = activate_channel (0, InitialState),
      NewState end,

  % Load per-channel idle slots for other channels.
  {ok, Channels} = application:get_env (camera_control, channels),
  lists:foreach (fun ({Channel, _, _}) ->
      if
        State#state.active_channel =/= Channel ->
          case database:action_lookup ("idle-" ++ atom_to_list (Channel)) of
            {ok, {load, Channel, IdleSlot}} ->
              ok = channel:load (Channel, IdleSlot);
            _ ->
              ok end;
        true ->
          ok end end,
    Channels),

  {ok, idle, State}.

terminate (_Reason, _StateName, _StateData) ->
  ok.


idle ({pressed, Button}, State) ->
  ActionId = if
    Button == State#state.last_button ->
      "idle";
    true ->
      button_id (Button) end,

  case database:action_lookup (ActionId) of
    {ok, {load, Channel, Slot}} ->
      {ok, NewState} = activate_slot (Channel, Slot, State),
      {next_state, idle, NewState#state{last_button=Button}};

    {ok, {channel, ChannelNum}} ->
      case activate_channel (ChannelNum, State) of
        {ok, NewState} ->
          {next_state, idle, NewState};
        _ ->
          {next_state, idle, State} end;

    {ok, {ptz, Direction}} ->
      ptz (Direction, State),
      {next_state, ptzing, State};

    {ok, save} ->
      error_logger:info_msg ("Please press the target button"),
      {next_state, saving_0, State, ?SAVE_TIMEOUT};

    _ ->
      {next_state, idle, State} end;

idle ({released, _Button}, State) ->
  {next_state, idle, State}.


ptzing ({pressed, _Button}, State) ->
  {next_state, ptzing, State};

ptzing ({released, _Button}, State) ->
  ptz (stop, State),
  {next_state, idle, State}.


saving_0 (timeout, State) ->
  error_logger:info_msg ("Timeout, save cancelled"),
  {next_state, idle, State};

saving_0 ({pressed, Button}, State) ->
  ButtonId = button_id (Button),

  case database:action_is_special (ButtonId) of
    false ->
      NewState = State#state{saving_id=ButtonId},
      error_logger:info_msg ("Please press save again"),
      {next_state, saving_1, NewState, ?SAVE_TIMEOUT};
    _ ->
      error_logger:info_msg ("Special button pressed, save cancelled"),
      {next_state, idle, State} end;

saving_0 ({released, _Button}, State) ->
  {next_state, saving_0, State, ?SAVE_TIMEOUT}.


saving_1 (timeout, State) ->
  error_logger:info_msg ("Timeout, save cancelled"),
  {next_state, idle, State#state{saving_id=false}};

saving_1 ({pressed, Button}, State) ->
  case database:action_lookup (button_id (Button)) of
    {ok, save} ->
      save (State);
    _ ->
      error_logger:info_msg ("Save cancelled"),
      void end,

  {next_state, idle, State#state{saving_id=false}};

saving_1 ({released, _Button}, State) ->
  {next_state, saving_1, State, ?SAVE_TIMEOUT}.


handle_event (_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event (_Event, _From, StateName, StateData) ->
  {reply, ok, StateName, StateData}.

handle_info (_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

code_change (_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.


%% private

activate_slot (Channel, Slot, State) ->
  ok = channel:load (Channel, Slot),
  ok = channel:activate (Channel),

  case State#state.active_channel of
    OldChannel when is_atom (OldChannel) andalso OldChannel =/= Channel ->
      Id = "idle-" ++ atom_to_list (OldChannel),

      case database:action_lookup (Id) of
        {ok, {load, OldChannel, IdleSlot}} ->
          ok = channel:load (OldChannel, IdleSlot);
        _ ->
          void end;

    _ ->
      void end,

  {ok, State#state{active_channel=Channel}}.

activate_channel (ChannelNum, State) ->
  {ok, Channels} = application:get_env (camera_control, channels),

  ChannelsLength = length (Channels),
  if
    ChannelNum < ChannelsLength ->
      {Channel, _, _} = lists:nth (ChannelNum+1, Channels),
      ok = channel:activate (Channel),
      {ok, State#state{active_channel=Channel}};
    true ->
      {error, no_such_channel} end.

ptz (Direction, State) ->
  channel:ptz (State#state.active_channel, Direction),
  ok.

save (State) ->
  Id = State#state.saving_id,
  Channel = State#state.active_channel,

  case database:action_lookup (Id, false) of
    {ok, {load, OldChannel, OldSlot}} ->
      channel:free (OldChannel, OldSlot);
    _ ->
      void end,

  {ok, {Channel, Slot}} = channel:save (Channel),
  database:action_set (Id, {load, Channel, Slot}, false),
  ok.

button_id ({Y, X}) ->
  "button-" ++ integer_to_list (Y) ++ "," ++ integer_to_list (X).

% vim:set et sw=2 sts=2:
