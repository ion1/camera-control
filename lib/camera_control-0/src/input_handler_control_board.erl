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

-module (input_handler_control_board).

-define (REGNAME, ?MODULE).

-define (SAVE_TIMEOUT, 1000).

-behaviour (gen_fsm).

%% API
-export ([start_link/0, pressed/1, released/1]).

%% gen_fsm callbacks
-export ([init/1, terminate/3, idle/2, ptzing/2, saving_0/2, saving_1/2,
          handle_event/3, handle_sync_event/4, handle_info/3, code_change/4]).

%% State
-record (state, {last_button=false,
                 saving_id=false}).


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

  {ok, idle, #state{}}.

terminate (_Reason, _StateName, _StateData) ->
  ok.


idle ({pressed, Button}, State) ->
  {ActionId, NewLastButton} = if
    Button == State#state.last_button ->
      {"idle", false};
    true ->
      {button_id (Button), Button} end,

  NewState = State#state{last_button=false},

  case db_actions:lookup (ActionId) of
    {ok, {load, _Channel, _Slot}=Action} ->
      ok = camera_control:activate (Action),
      {next_state, idle, NewState#state{last_button=NewLastButton}};

    {ok, {ptz, _Direction}=Action} ->
      ok = camera_control:activate (Action),
      {next_state, ptzing, NewState};

    {ok, save} ->
      error_logger:info_msg ("Please press the target button"),
      {next_state, saving_0, NewState, ?SAVE_TIMEOUT};

    {ok, Action} ->
      ok = camera_control:activate (Action),
      {next_state, idle, NewState};

    _ ->
      {next_state, idle, NewState} end;

idle ({released, _Button}, State) ->
  {next_state, idle, State}.


ptzing ({pressed, _Button}, State) ->
  {next_state, ptzing, State};

ptzing ({released, _Button}, State) ->
  ok = camera_control:activate ({ptz, stop}),
  {next_state, idle, State}.


saving_0 (timeout, State) ->
  error_logger:info_msg ("Timeout, save cancelled"),
  {next_state, idle, State};

saving_0 ({pressed, Button}, State) ->
  ButtonId = button_id (Button),

  case db_actions:is_special (ButtonId) of
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
  case db_actions:lookup (button_id (Button)) of
    {ok, save} ->
      camera_control:save (State#state.saving_id);
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

button_id ({Y, X}) ->
  "button-" ++ integer_to_list (Y) ++ "," ++ integer_to_list (X).

% vim:set et sw=2 sts=2:
