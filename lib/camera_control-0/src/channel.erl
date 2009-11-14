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

-module (channel).

-export ([activate/1, ptz/2, free/2, load/2, save/2]).

activate (Id) ->
  error_logger:info_msg ("~p: activate", [Id]),
  gen_fsm:sync_send_event (Id, activate).

ptz (Id, stop) ->
  error_logger:info_msg ("~p: ptz stop", [Id]),
  gen_fsm:sync_send_event (Id, {ptz, stop});

ptz (Id, {P, T, Z}) ->
  error_logger:info_msg ("~p: ptz ~p", [Id, {P, T, Z}]),
  gen_fsm:sync_send_event (Id, {ptz, {P, T, Z}}).

free (Id, ChannelSlot) ->
  error_logger:info_msg ("~p: free ~p", [Id, ChannelSlot]),
  gen_fsm:sync_send_event (Id, {free, ChannelSlot}).

load (Id, ChannelSlot) ->
  error_logger:info_msg ("~p: load ~p", [Id, ChannelSlot]),
  gen_fsm:sync_send_event (Id, {load, ChannelSlot}).

save (Id, ActionId) ->
  error_logger:info_msg ("~p: save ~p", [Id, ActionId]),
  case gen_fsm:sync_send_event (Id, {save, ActionId}) of
    {ok, ChannelSlot} ->
      {ok, Id, ChannelSlot};
    {error, Message} ->
      {error, Message} end.

% vim:set et sw=2 sts=2:
