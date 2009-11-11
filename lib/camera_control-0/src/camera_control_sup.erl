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
