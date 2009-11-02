-module (database).

-define (REGNAME, ?MODULE).

-behaviour (gen_server).

%% API
-export ([start_link/0, dump/0, action_lookup/2, action_lookup/1,
          action_is_special/1, action_set/3, ssp_camera_init/1,
          ssp_camera_slot_free/2, ssp_camera_slot_is_allocated/2,
          ssp_camera_slot_allocate/1]).

%% gen_server callbacks
-export ([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3]).

%% includes
-include ("database.hrl").
-include ("ssp_constants.hrl").

%% State
-record (state, {dir, actions, ssp_camera_slots}).


%% API

start_link () ->
  gen_server:start_link ({local, ?REGNAME}, ?MODULE, dummy, []).

dump () ->
  gen_server:call (?REGNAME, dump).

action_lookup (Id, Unref) ->
  gen_server:call (?REGNAME, {action_lookup, Id, Unref}).

action_lookup (Id) ->
  action_lookup (Id, true).

action_is_special (Id) ->
  gen_server:call (?REGNAME, {action_is_special, Id}).

action_set (Id, Action, Special) ->
  gen_server:call (?REGNAME, {action_set, Id, Action, Special}).

ssp_camera_init (Addr) ->
  gen_server:call (?REGNAME, {ssp_camera_init, Addr}).

ssp_camera_slot_free (Addr, Slot) ->
  gen_server:call (?REGNAME, {ssp_camera_slot_free, Addr, Slot}).

ssp_camera_slot_is_allocated (Addr, Slot) ->
  gen_server:call (?REGNAME, {ssp_camera_slot_is_allocated, Addr, Slot}).

ssp_camera_slot_allocate (Addr) ->
  gen_server:call (?REGNAME, {ssp_camera_slot_allocate, Addr}).


%% gen_server callbacks

init (dummy) ->
  process_flag (trap_exit, true),

  {ok, Dir} = application:get_env (camera_control, database_dir),

  {ok, Actions} = read_table (actions, Dir),
  {ok, SspCameraSlots} = read_table (ssp_camera_slots, Dir),

  {ok, #state{dir=Dir, actions=Actions, ssp_camera_slots=SspCameraSlots}}.

terminate (_Reason, _State) ->
  ok.


handle_call (dump, _From, State) ->
  dump_table (State#state.actions),
  dump_table (State#state.ssp_camera_slots),
  {reply, ok, State};


handle_call ({action_lookup, Id, Unref}, _From, State) ->
  Table = State#state.actions,

  Result = case action_lookup_maybe_unref (Table, Id, Unref) of
    {ok, Entry} ->
      {ok, Entry#action.action};
    _ ->
      {error, not_found} end,

  {reply, Result, State};

handle_call ({action_is_special, Id}, _From, State) ->
  Table = State#state.actions,

  Result = case ets:lookup (Table, Id) of
    [Entry] when Entry#action.special ->
      true;
    _ ->
      false end,

  {reply, Result, State};

handle_call ({action_set, Id, Action, Special}, _From, State) ->
  error_logger:info_msg ("Action: set ~p: ~p", [Id, Action]),

  Table = State#state.actions,

  ets:insert (Table, #action{id=Id, action=Action, special=Special}),
  ok = write_table (Table, State#state.dir),

  {reply, ok, State};


handle_call ({ssp_camera_init, Addr}, _From, State) ->
  Table = State#state.ssp_camera_slots,

  case ets:match_object (Table,
                         #ssp_camera_slot{id='_', camera_addr=Addr,
                                          slot='_', allocated='_'},
                         1) of
    '$end_of_table' ->
      % No slots exist for the camera.  Create them.
      error_logger:info_msg ("SSP camera ~p: Initializing database", [Addr]),

      lists:foreach (fun (Slot) ->
          Id = ssp_camera_slot_id (Addr, Slot),
          ets:insert (Table, #ssp_camera_slot{id=Id, camera_addr=Addr,
                                              slot=Slot}) end,
        lists:seq (?SSP_PRESET_MIN, ?SSP_PRESET_MAX)),

      ok = write_table (Table, State#state.dir);

    _ ->
      void end,
  {reply, ok, State};

handle_call ({ssp_camera_slot_free, Addr, Slot}, _From, State) ->
  Table = State#state.ssp_camera_slots,
  Id = ssp_camera_slot_id (Addr, Slot),

  error_logger:info_msg ("SSP camera ~p: Freeing slot ~p", [Addr, Slot]),

  [Entry] = ets:lookup (Table, Id),
  ets:insert (Table, Entry#ssp_camera_slot{allocated=false}),
  ok = write_table (Table, State#state.dir),

  {reply, ok, State};

handle_call ({ssp_camera_slot_is_allocated, Addr, Slot}, _From, State) ->
  Table = State#state.ssp_camera_slots,
  Id = ssp_camera_slot_id (Addr, Slot),

  Result = case ets:lookup (Table, Id) of
    [Entry] when Entry#ssp_camera_slot.allocated ->
      true;
    [] ->
      false end,

  {reply, Result, State};

handle_call ({ssp_camera_slot_allocate, Addr}, _From, State) ->
  Table = State#state.ssp_camera_slots,

  Result = case ets:match_object (Table,
                                  #ssp_camera_slot{id='_', camera_addr=Addr,
                                                   slot='_', allocated=false},
                                  1) of
    {[Entry], _} ->
      error_logger:info_msg ("SSP camera ~p: Allocating slot ~p",
                             [Addr, Entry#ssp_camera_slot.slot]),

      ets:insert (Table, Entry#ssp_camera_slot{allocated=true}),
      ok = write_table (Table, State#state.dir),

      {ok, Entry#ssp_camera_slot.slot};

    '$end_of_table' ->
      {error, no_free_slots} end,

  {reply, Result, State}.


handle_cast (_Request, State) ->
  {noreply, State}.

handle_info (_Info, State) ->
  {noreply, State}.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.


%% private

read_table (Name, Dir) ->
  Filename = atom_to_list (Name),

  Path = filename:join (Dir, Filename++".table"),

  error_logger:info_msg ("Loading ~p", [Path]),

  {Exists, List} = case file:consult (Path) of
    {ok, [Data]} ->
      {true, Data};
    {error, enoent} ->
      {false, []} end,

  Table = ets:new (Name, [{keypos, 2}]),
  ets:insert (Table, List),

  if
    not Exists ->
      ok = write_table (Table, Dir);
    true ->
      ok end,

  {ok, Table}.

write_table (Table, Dir) ->
  Name = table_name (Table),
  Filename = atom_to_list (Name),

  Path = filename:join (Dir, Filename++".table"),
  TempPath = Path ++ ".new",

  error_logger:info_msg ("Saving ~p", [Path]),

  ok = filelib:ensure_dir (Path),

  try
    {ok, Io} = file:open (TempPath, [write, binary]),
    try
      ok = io:format (Io, "~p.~n", [ets:tab2list (Table)]),
      ok = file:sync (Io)
    after
      file:close (Io) end,

    ok = file:rename (TempPath, Path)

  after
    file:delete (TempPath) end.

dump_table (Table) ->
  io:format ("Table ~p:~n~p~n~n", [table_name (Table), ets:tab2list (Table)]),
  ok.

table_name (Table) ->
  {name, Name} = lists:keyfind (name, 1, ets:info (Table)),
  Name.

action_lookup_maybe_unref (Table, Id, Unref) ->
  case ets:lookup (Table, Id) of
    [Entry] ->
      error_logger:info_msg ("Action: ~p: ~p", [Id, Entry#action.action]),

      case Entry#action.action of
        {ref, NewId} ->
          if
            Unref ->
              action_lookup_maybe_unref (Table, NewId, Unref);
            true ->
              {ok, Entry} end;

        _ ->
          {ok, Entry} end;

    _ ->
      error_logger:info_msg ("Action: ~p: Not found", [Id]),
      {error, not_found} end.

ssp_camera_slot_id (Addr, Slot) ->
  integer_to_list (Addr) ++ "-" ++ integer_to_list (Slot).

% vim:set et sw=2 sts=2:
