-module (camera_control_app).

-behaviour (application).

-export ([start/2, stop/1]).

start (_Type, []) ->
  camera_control_sup:start_link ().

stop (_State) ->
  ok.

% vim:set et sw=2 sts=2:
