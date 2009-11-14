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

-module (db_actions).

-define (REGNAME, ?MODULE).

-behaviour (gen_server).

%% API
-export ([start_link/0, dump/0, lookup/2, lookup/1, is_special/1, set/5,
          set_raw/3]).

%% gen_server callbacks
-export ([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3]).

%% includes
-include ("db_actions.hrl").
-include ("lazy.hrl").

%% State
-record (state, {dir, table}).


%% API

start_link () ->
  gen_server:start_link ({local, ?REGNAME}, ?MODULE, dummy, []).

dump () ->
  gen_server:call (?REGNAME, dump).

lookup (Id, Deref) ->
  gen_server:call (?REGNAME, {lookup, Id, Deref}).

lookup (Id) ->
  lookup (Id, true).

is_special (Id) ->
  gen_server:call (?REGNAME, {is_special, Id}).

set (Id, Channel, FreeSlot, PickSlot, SaveSlot) ->
  gen_server:call (?REGNAME, {set, Id, Channel, FreeSlot, PickSlot, SaveSlot}).

set_raw (Id, Action, Special) ->
  gen_server:call (?REGNAME, {set_raw, Id, Action, Special}).



%% gen_server callbacks

init (dummy) ->
  process_flag (trap_exit, true),

  {ok, Dir} = application:get_env (camera_control, database_dir),

  {ok, Table} = db:read (actions, Dir),

  {ok, #state{dir=Dir, table=Table}}.

terminate (_Reason, _State) ->
  ok.


handle_call (dump, _From, #state{table=Table} = State) ->
  db:dump (Table),
  {reply, ok, State};


handle_call ({lookup, Id, Deref}, _From, #state{table=Table} = State) ->
  Result = case lookup_maybe_deref (Table, Id, Deref) of
    {ok, Entry} ->
      {ok, Entry#action.action};
    _ ->
      {error, not_found} end,

  {reply, Result, State};

handle_call ({is_special, Id}, _From, #state{table=Table} = State) ->
  Result = case ets:lookup (Table, Id) of
    [Entry] when Entry#action.special ->
      true;
    _ ->
      false end,

  {reply, Result, State};

handle_call ({set, Id, Channel, FreeSlot, PickSlot, SaveSlot}, _From, State) ->
  Table = State#state.table,

  OldEntry = case ets:lookup (Table, Id) of
    [Entry] ->
      Entry;
    _ ->
      false end,

  Res = case OldEntry of
    #action{special=true} ->
      {error, action_is_special};

    _ ->
      % Free the old entry.
      OldSlot = case OldEntry of
        #action{action={load, Channel, OSlot}} ->
          ok = FreeSlot (OSlot),
          OSlot;
        #action{action={load, OtherChannel, OSlot}} ->
          ok = channel:free (OtherChannel, OSlot),
          false;
        _ ->
          false end,

      TakenSlots = ?DELAY (ets:foldl (fun (Entry, TSlots) ->
          case Entry of
            #action{action={load, Channel, TSlot}} when TSlot =/= OldSlot ->
              ordsets:add_element (TSlot, TSlots);
            _ ->
              TSlots end end,
        ordsets:new (), Table)),

      {ok, NewSlot} = PickSlot (TakenSlots, OldSlot),

      Action = {load, Channel, NewSlot},
      error_logger:info_msg ("Action: set ~p: ~p", [Id, Action]),
      ets:insert (Table, #action{id=Id, action=Action, special=false}),
      ok = SaveSlot (NewSlot),
      ok = db:write (Table, State#state.dir),

      {ok, NewSlot} end,

  {reply, Res, State};

handle_call ({set_raw, Id, Action, Special}, _From, State) ->
  Table = State#state.table,

  error_logger:info_msg ("Action: set ~p: ~p", [Id, Action]),

  ets:insert (Table, #action{id=Id, action=Action, special=Special}),
  ok = db:write (Table, State#state.dir),

  {reply, ok, State}.


handle_cast (_Request, State) ->
  {noreply, State}.

handle_info (_Info, State) ->
  {noreply, State}.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.


%% private

lookup_maybe_deref (Table, Id, Deref) ->
  lookup_maybe_deref (Table, Id, Deref, []).

lookup_maybe_deref (Table, Id, Deref, ParentRefs) ->
  NewParentRefs = [Id|ParentRefs],

  case ets:lookup (Table, Id) of
    [Entry] ->
      case Entry#action.action of
        {ref, NewId} when Deref ->
          case lists:member (NewId, NewParentRefs) of
            false ->
              lookup_maybe_deref (Table, NewId, Deref, NewParentRefs);
            _ ->
              error_logger:info_msg ("Infinite loop: ~p",
                                     [[NewId|NewParentRefs]]),
              {error, infinite_loop} end;

        _ ->
          {ok, Entry} end;

    _ ->
      {error, not_found} end.

% vim:set et sw=2 sts=2:
