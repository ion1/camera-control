{application,
 camera_control,
 [{description, "Camera Control"},
  {vsn, "0"},
  {modules, [camera_control,
             camera_control_app,
             camera_control_sup,
             channel_sup,
             control_board,
             control_board_scanner,
             database,
             ssp]},
  {registered, [camera_control,
                camera_control_sup,
                channel_sup,
                control_board,
                control_board_scanner,
                database,
                ssp]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {camera_control_app, []}}]}.

% vim:set et sw=2 sts=2: