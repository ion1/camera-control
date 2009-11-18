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

-module (db_channel_transactions).

-export([save/2]).

-record (save_state, {new_action=false, orig_action=false}).

-include ("lazy.hrl").

save (Channel, ActionId) ->
  transaction:run (#save_state{}, [fun (State) ->
      case db_actions:is_special (ActionId) of
        true ->
          {error, action_is_special};
        _ ->
          {ok, State} end end,

    fun (State) ->
      OrigAction = case db_actions:lookup (ActionId, false) of
        {ok, {load, _, _}=Action} ->
          Action;
        _ ->
          false end,

      {ok, State#save_state{orig_action=OrigAction}} end,

    fun (State) ->
      {ok, NewSlot} = channel:save (Channel, ?DELAY (begin
        {ok, TakenSlots} = db_actions:taken_slots (Channel),
        TakenSlots end)),

      NewAction = {load, Channel, NewSlot},

      Rollback = fun () -> channel:free (Channel, NewSlot), ok end,

      {ok, State#save_state{new_action=NewAction}, Rollback} end,

    fun (#save_state{new_action=NewAction, orig_action=OrigAction} = State) ->
      db_actions:set (ActionId, NewAction, false),

      Rollback = fun () ->
        db_actions:set (ActionId, OrigAction, false),
        ok end,

      {ok, State, Rollback} end,

    fun (#save_state{orig_action=OrigAction} = State) ->
      case OrigAction of
        {load, OrigChannel, OrigSlot} ->
          channel:free (OrigChannel, OrigSlot);
        _ ->
          void end,

      {ok, State} end]).

% vim:set et sw=2 sts=2:
