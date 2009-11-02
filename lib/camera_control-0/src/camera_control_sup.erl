-module (camera_control_sup).

-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% supervisor callbacks
-export ([init/1]).


%% API

start_link () ->
  supervisor:start_link (?MODULE, dummy).


%% supervisor callbacks

init (dummy) ->
  process_flag (trap_exit, true),

  Children = [
    child (database, '', [], worker),

    % depends on database
    child (channel_sup, '', [], supervisor),

    child (ssp, '', ["/dev/ttyS0"], worker),

    % depends on channel_sup
    child (control_board, '', ["/dev/parport0"], worker),

    % depends on database, channel_sup
    child (camera_control, '', [], worker),

    % depends on control_board, camera_control
    child (control_board_scanner, '', [], worker)],

  {ok, {{rest_for_one, 3, 10}, Children}}.


%% private

child (Id, Mod0, Args, Type) ->
  Mod = case Mod0 of
    '' -> Id;
    _ -> Mod0 end,

  Modules = case Mod of
    gen_event -> dynamic;
    _ -> [Mod] end,

  Shutdown = case Type of
    supervisor -> infinity;
    _ -> 1000 end,

  {Id, {Mod, start_link, Args}, permanent, Shutdown, Type, Modules}.

% vim:set et sw=2 sts=2:
