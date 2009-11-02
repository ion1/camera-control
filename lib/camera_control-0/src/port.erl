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
