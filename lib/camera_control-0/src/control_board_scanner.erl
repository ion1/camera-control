-module (control_board_scanner).

-define (REGNAME, ?MODULE).

-define (SCAN_PERIOD, 20).

-behaviour (gen_fsm).

%% API
-export ([start_link/0, handle_event/3, handle_sync_event/4, handle_info/3,
          code_change/4]).

%% gen_fsm callbacks
-export ([init/1, terminate/3, main/2]).

%% State
-record (state, {button=false}).


%% API

start_link () ->
  gen_fsm:start_link ({local, ?REGNAME}, ?MODULE, dummy, []).


%% gen_fsm callbacks

init (dummy) ->
  process_flag (trap_exit, true),
  {ok, main, #state{}, 0}.

terminate (_Reason, _StateName, _StateData) ->
  ok.


main (timeout, State) ->
  PrevButton = State#state.button,

  {ok, Buttons} = control_board:read_input (),

  NewState = case Buttons of
    [] ->
      if
        PrevButton =/= false ->
          ok = notify_released (PrevButton),
          State#state{button=false};
        true ->
          State end;

    [Button] ->
      if
        PrevButton == false ->
          ok = notify_pressed (Button),
          State#state{button=Button};

        PrevButton =/= Button ->
          ok = notify_released (PrevButton),
          ok = notify_pressed (Button),
          State#state{button=Button};

        true ->
          State end;

    Buttons ->
      if
        PrevButton =/= false ->
          % The hardware reports invalid data if multiple buttons are pressed
          % simultaneously.  We can only tell somewhat reliably if a button was
          % released.
          ProbablyReleased = not lists:member (PrevButton, Buttons),

          if
            ProbablyReleased ->
              ok = notify_released (PrevButton),
              State#state{button=false};
            true ->
              State end;
        true ->
          State end end,

  {next_state, main, NewState, ?SCAN_PERIOD}.


handle_event (_Event, StateName, StateData) ->
  {next_state, StateName, StateData, ?SCAN_PERIOD}.

handle_sync_event (_Event, _From, StateName, StateData) ->
  {reply, ok, StateName, StateData, ?SCAN_PERIOD}.

handle_info (_Info, StateName, StateData) ->
  {next_state, StateName, StateData, ?SCAN_PERIOD}.

code_change (_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.


%% private

notify_pressed (Button) ->
  error_logger:info_msg ("Press   ~p", [Button]),
  ok = camera_control:pressed (Button).

notify_released (Button) ->
  error_logger:info_msg ("Release ~p", [Button]),
  ok = camera_control:released (Button).

% vim:set et sw=2 sts=2: