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

-module (port).

-export ([open/2, close/1, send/2, get_response/1]).

open (Path, Args) ->
  open_port ({spawn_executable, Path},
             [{args, Args}, {packet, 2}, binary, exit_status]).

close (Port) ->
  port_close (Port).

send (Port, Request) ->
  port_command (Port, term_to_binary (Request)),
  case get_response (Port) of
    {response, Response} ->
      Response end.

get_response (Port) ->
  receive
    {Port, {data, Data}} ->
      {response, binary_to_term (Data)} end.

% vim:set et sw=2 sts=2:
