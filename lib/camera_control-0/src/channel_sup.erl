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

  {ok, {{one_for_one, 3, 10}, Children}}.

% vim:set et sw=2 sts=2:
