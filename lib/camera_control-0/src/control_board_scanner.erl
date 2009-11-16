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
        PrevButton =:= false ->
          State end;

    [Button] ->
      if
        PrevButton =:= false ->
          ok = notify_pressed (Button),
          State#state{button=Button};

        PrevButton =/= Button ->
          ok = notify_released (PrevButton),
          ok = notify_pressed (Button),
          State#state{button=Button};

        PrevButton =:= Button ->
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
            not ProbablyReleased ->
              State end;
        PrevButton =:= false ->
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
  ok = input_handler_control_board:pressed (Button).

notify_released (Button) ->
  error_logger:info_msg ("Release ~p", [Button]),
  ok = input_handler_control_board:released (Button).

% vim:set et sw=2 sts=2:
