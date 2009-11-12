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

-module (camera_control).

-define (REGNAME, ?MODULE).

-behaviour (gen_server).

%% API
-export ([start_link/0, activate/1, save/1]).

%% gen_server callbacks
-export ([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3]).

%% State
-record (state, {active_channel=false}).


%% API

start_link () ->
  gen_server:start_link ({local, ?REGNAME}, ?MODULE, dummy, []).

activate (Action) ->
  gen_server:call (?REGNAME, {activate, Action}).

save (Id) ->
  gen_server:call (?REGNAME, {save, Id}).


%% gen_server callbacks

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

  {ok, State}.

terminate (_Reason, _State) ->
  ok.


handle_call ({activate, {load, Channel, Slot}}, _From, State) ->
  {ok, NewState} = activate_slot (Channel, Slot, State),
  {reply, ok, NewState};

handle_call ({activate, {channel, ChannelNum}}, _From, State) ->
  case activate_channel (ChannelNum, State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    _ ->
      {reply, ok, State} end;

handle_call ({activate, {ptz, Direction}}, _From, State) ->
  channel:ptz (State#state.active_channel, Direction),
  {reply, ok, State};


handle_call ({save, Id}, _From, State) ->
  save (Id, State),
  {reply, ok, State}.


handle_cast (_Request, State) ->
  {noreply, State}.

handle_info (_Info, State) ->
  {noreply, State}.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.


%% private

activate_slot (Channel, Slot, State) ->
  ok = channel:load (Channel, Slot),
  ok = channel:activate (Channel),

  % Tell the previous camera to go to idle position.
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

free (Id, _State) ->
  case database:action_lookup (Id, false) of
    {ok, {load, Channel, Slot}} ->
      channel:free (Channel, Slot);
    _ ->
      void end,

  ok.

save (Id, #state{active_channel=Channel} = State) ->
  case database:action_is_special (Id) of
    true ->
      {error, action_is_special};

    _ ->
      free (Id, State),

      {ok, Channel, Slot} = channel:save (Channel),
      database:action_set (Id, {load, Channel, Slot}, false),
      ok end.

% vim:set et sw=2 sts=2:
