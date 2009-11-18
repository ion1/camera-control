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

-module (channel_ssp_camera).

-define (PTZ_TIMEOUT,  50).

-behaviour (gen_fsm).

%% API
-export ([start_link/3]).

%% gen_fsm callbacks
-export ([init/1, terminate/3, idle/3, ptzing/2, ptzing/3, handle_event/3,
         handle_sync_event/4, handle_info/3, code_change/4]).

%% includes
-include ("lazy.hrl").
-include ("ssp_constants.hrl").

%% State
-record (state, {camera_addr, channel_num, ptz=stop}).


%% API

start_link (Id, CameraAddr, ChannelNum) ->
  gen_fsm:start_link ({local, Id}, ?MODULE, {CameraAddr, ChannelNum}, []).


%% gen_fsm callbacks

init ({CameraAddr, ChannelNum}) ->
  process_flag (trap_exit, true),

  {ok, idle, #state{camera_addr=CameraAddr, channel_num=ChannelNum}}.

terminate (_Reason, _StateName, _StateData) ->
  ok.


idle (activate, _From, State) ->
  ok = control_board:select_video (State#state.channel_num),
  {reply, ok, idle, State};

idle ({ptz, stop}, _From, State) ->
  {reply, ok, idle, State};

idle ({ptz, {P, T, Z}}, _From, State) ->
  {reply, ok, ptzing, ptz ({P, T, Z}, State), ?PTZ_TIMEOUT};

idle ({load, Slot}, _From, State) ->
  ok = load (Slot, State),
  {reply, ok, idle, State};

idle ({free, Slot}, _From, State) ->
  ok = free (Slot, State),
  {reply, ok, idle, State};

idle ({save, TakenSlotsPromise}, _From, State) ->
  {ok, Slot} = save (TakenSlotsPromise, State),
  {reply, {ok, Slot}, idle, State}.


ptzing (timeout, State) ->
  {next_state, ptzing, ptz (State), ?PTZ_TIMEOUT}.

ptzing ({ptz, {P, T, Z}}, _From, State) ->
  {reply, ok, ptzing, ptz ({P, T, Z}, State), ?PTZ_TIMEOUT};

ptzing (Event, From, State) ->
  idle (Event, From, ptz (stop, State)).


handle_event (_Event, StateName, StateData) ->
  case StateName of
    ptzing ->
      {next_state, StateName, StateData, ?PTZ_TIMEOUT};
    _ ->
      {next_state, StateName, StateData} end.

handle_sync_event (_Event, _From, StateName, StateData) ->
  case StateName of
    ptzing ->
      {reply, ok, StateName, StateData, ?PTZ_TIMEOUT};
    _ ->
      {reply, ok, StateName, StateData} end.

handle_info (_Info, StateName, StateData) ->
  case StateName of
    ptzing ->
      {next_state, StateName, StateData, ?PTZ_TIMEOUT};
    _ ->
      {next_state, StateName, StateData} end.

code_change (_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.


%% private

ptz (stop, State) -> 
  ssp_camera:ptz (State#state.camera_addr, {0, 0, 0}),
  State#state{ptz=stop};

ptz ({P, T, Z}, State) ->
  ssp_camera:ptz (State#state.camera_addr, {4*P, 2*T, Z}),
  State#state{ptz={P, T, Z}}.

ptz (State) ->
  ptz (State#state.ptz, State),
  State.

load (Slot, State) ->
  ssp_camera:preset_goto (State#state.camera_addr, Slot),
  ok.

free (_Slot, _State) ->
  % Nothing needs to be sent to the camera.
  ok.

save (TakenSlotsPromise, State) ->
  AllSlots = ordsets:from_list (lists:seq (?SSP_PRESET_MIN, ?SSP_PRESET_MAX)),
  [Slot | _] = ordsets:subtract (AllSlots, ?FORCE (TakenSlotsPromise)),

  ok = ssp_camera:preset_set (State#state.camera_addr, Slot),

  % A little notification of a successful save. The cameras seem to ignore PTZ
  % commands for a couple hundred milliseconds just after saving. 400 ms of
  % sending a movement request seems to work quite nicely, taking into account
  % that the first part of it doesn’t do anything.
  lists:foreach (fun (_) ->
      ssp_camera:ptz (State#state.camera_addr, {1, 0, 0}),
      timer:sleep (?PTZ_TIMEOUT) end,
    lists:seq (0, trunc (400/?PTZ_TIMEOUT))),
  ssp_camera:ptz (State#state.camera_addr, {0, 0, 0}),
  ssp_camera:preset_goto (State#state.camera_addr, Slot),

  {ok, Slot}.

% vim:set et sw=2 sts=2:
