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

-module (db).

-export ([read/2, write/2, table_name/1, dump/1]).

read (Name, Dir) ->
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
      ok = write (Table, Dir);
    true ->
      ok end,

  {ok, Table}.

write (Table, Dir) ->
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

table_name (Table) ->
  {name, Name} = lists:keyfind (name, 1, ets:info (Table)),
  Name.

dump (Table) ->
  io:format ("Table ~p:~n~p~n~n", [table_name (Table), ets:tab2list (Table)]),
  ok.

% vim:set et sw=2 sts=2:
