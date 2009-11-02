-module (channel_sup).

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

  {ok, Channels} = application:get_env (channels),

  Children = lists:map (fun ({Id, Module, Args}) ->
      RealModule = list_to_atom ("channel_" ++ atom_to_list (Module)),
      {Id, {RealModule, start_link, [Id | Args]},
       permanent, 1000, worker, [RealModule]} end,
    Channels),

  {ok, {{one_for_all, 1, 5}, Children}}.

% vim:set et sw=2 sts=2:
