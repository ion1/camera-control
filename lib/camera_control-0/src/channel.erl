-module (channel).

-export ([activate/1, ptz/2, free/2, load/2, save/1]).

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

save (Id) ->
  error_logger:info_msg ("~p: save", [Id]),
  case gen_fsm:sync_send_event (Id, save) of
    {ok, ChannelSlot} ->
      {ok, Id, ChannelSlot};
    {error, Message} ->
      {error, Message} end.

% vim:set et sw=2 sts=2:
