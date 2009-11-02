% First address byte: categories
-define(SSP_ADDR_CAT_ALL,         16#f0).
-define(SSP_ADDR_CAT_CONTROLLER,  16#f1).
-define(SSP_ADDR_CAT_MULTIPLEXER, 16#f2).
-define(SSP_ADDR_CAT_VCR,         16#f3).
-define(SSP_ADDR_CAT_CAMERA,      16#f4).
-define(SSP_ADDR_CAT_DVR,         16#f5).
-define(SSP_ADDR_CAT_OTHER,       16#f7).

% Second address byte: devices
-define(SSP_ADDR_DEV_MIN, 16#00).
-define(SSP_ADDR_DEV_MAX, 16#7f).

-define(SSP_ADDR_DEV_CONTROLLER_MIN, 16#00).
-define(SSP_ADDR_DEV_CONTROLLER_MAX, 16#04).

-define(SSP_ADDR_DEV_BROADCAST,           16#e0).
-define(SSP_ADDR_DEV_BROADCAST_GROUP_MIN, 16#e1).
-define(SSP_ADDR_DEV_BROADCAST_GROUP_MAX, 16#ef).

% Memory slots
-define(SSP_PRESET_MIN, 16#01).
-define(SSP_PRESET_MAX, 16#7f).

% Commands
-define(SSP_ACK,                  16#0a).
-define(SSP_CONCLUSION,           16#1e).
-define(SSP_CONTINUOUS,           16#1f).
-define(SSP_ENTER,                16#40).
-define(SSP_MENU,                 16#74).
-define(SSP_OSD_EXIT,             16#8e).
-define(SSP_PAN_DIRECT,           16#42).
-define(SSP_PRESET_MEMORY,        16#26).
-define(SSP_PRESET_POSITION,      16#b8).
-define(SSP_PTZ_CONTROL,          16#25).
-define(SSP_RECEIVE_CONFIRMATION, 16#fe).
-define(SSP_SCREEN_SHOT,          16#45).
-define(SSP_SHIFT_DOWN,           16#64).
-define(SSP_SHIFT_LEFT,           16#53).
-define(SSP_SHIFT_RIGHT,          16#63).
-define(SSP_SHIFT_UP,             16#54).
-define(SSP_TILT_DIRECT,          16#43).
-define(SSP_TRANSMISSION_START,   16#fd).
-define(SSP_ZOOM_DIRECT,          16#44).

% Delays
-define(SSP_DELAY_BUS_FREE,    100).
-define(SSP_DELAY_RELEASE_BUS, 200).
-define(SSP_DELAY_BYTES,        40).

% vim:set et sw=2 sts=2:
