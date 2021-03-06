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

-module (channel_passthrough).

-behaviour (gen_fsm).

%% API
-export ([start_link/2]).

%% gen_fsm callbacks
-export ([init/1, terminate/3, idle/3, handle_event/3, handle_sync_event/4,
          handle_info/3, code_change/4]).

%% State
-record (state, {channel_num}).


%% API

start_link (Id, ChannelNum) ->
  gen_fsm:start_link ({local, Id}, ?MODULE, ChannelNum, []).


%% gen_fsm callbacks

init (ChannelNum) ->
  process_flag (trap_exit, true),
  {ok, idle, #state{channel_num=ChannelNum}}.

terminate (_Reason, _StateName, _StateData) ->
  ok.


idle (activate, _From, State) ->
  ok = control_board:select_video (State#state.channel_num),
  {reply, ok, idle, State};

idle ({ptz, _Direction}, _From, State) ->
  {reply, ok, idle, State};

idle ({load, _Slot}, _From, State) ->
  ok = control_board:select_video (State#state.channel_num),
  {reply, ok, idle, State};

idle ({free, _Slot}, _From, State) ->
  {reply, ok, idle, State};

idle ({save, _TakenSlotsPromise}, _From, State) ->
  {reply, {ok, dummy}, idle, State}.


handle_event (_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event (_Event, _From, StateName, StateData) ->
  {reply, ok, StateName, StateData}.

handle_info (_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

code_change (_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

% vim:set et sw=2 sts=2:
