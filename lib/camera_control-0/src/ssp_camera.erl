-module (ssp_camera).

%% API
-export ([preset_goto/2, preset_set/2, menu/1, enter/1, ptz/2]).

%% includes
-include ("ssp_constants.hrl").


%% API

preset_goto (Addr, Preset) ->
  ssp:send (?SSP_ADDR_CAT_CAMERA, Addr, [<<?SSP_PRESET_POSITION, Preset>>]).

preset_set (Addr, Preset) ->
  ssp:send (?SSP_ADDR_CAT_CAMERA, Addr, [<<?SSP_PRESET_MEMORY, Preset>>]).

menu (Addr) ->
  ssp:send (?SSP_ADDR_CAT_CAMERA, Addr, [<<?SSP_MENU>>]).

enter (Addr) ->
  ssp:send (?SSP_ADDR_CAT_CAMERA, Addr, [<<?SSP_ENTER>>]).

ptz (Addr, {P0, T0, Z0}) ->
  P = clip (P0, -7, 7),
  T = clip (T0, -7, 7),
  Z = clip (Z0, -1, 1),

  Right = bool_to_int (P > 0),
  Left  = bool_to_int (P < 0),
  Up    = bool_to_int (T < 0),
  Down  = bool_to_int (T > 0),
  Out   = bool_to_int (Z < 0),
  In    = bool_to_int (Z > 0),

  Data = <<0:4, Down:1, Up:1, Left:1, Right:1,
           In:1, Out:1,  (abs (T)):3,  (abs (P)):3>>,

  Cmd = case Data of
    <<0:16>> ->
      <<?SSP_CONCLUSION>>;
    <<_:16>> ->
      <<?SSP_PTZ_CONTROL, Data/bits>> end,

  ssp:send (?SSP_ADDR_CAT_CAMERA, Addr, [Cmd]).


%% private

clip (Num, Min, Max) ->
  erlang:min (Max, erlang:max (Min, Num)).

bool_to_int (Arg) when Arg -> 1;
bool_to_int (_)            -> 0.

% vim:set et sw=2 sts=2:
