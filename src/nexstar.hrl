-ifndef(__NEXSTAR_HRL__).
-define(__NEXSTAR_HRL__, true).

-define(AZM, 16#10).  %% 16
-define(ALT, 16#11).  %% 17
-define(GPS, 16#b0).  %% 176
-define(RTC, 16#b2).  %% 178

-define(DEVICE_MAP,
	?MAP(any,16#00),
	?MAP(main,16#01),
	?MAP(hc,16#04),
	?MAP(hcplus,16#0d),
	?MAP(azm,?AZM),
	?MAP(alt,?ALT),
	?MAP(app,16#20),
	?MAP(gps,?GPS),
	?MAP(rtc,?RTC),
	?MAP(wifly,16#b5),
	?MAP(power,16#b6),
	?MAP(bat, 16#b7),
	?MAP(light,16#bf)).

-define(NORTH, 16#0b).
-define(EAST,  16#0d).
-define(SOUTH, 16#0e).
-define(WEST,  16#07).

-define(COMPASS_MAP,
	?MAP(north,?NORTH),
	?MAP(north_east,?NORTH band ?EAST),
	?MAP(east,?EAST),
	?MAP(south_east,?SOUTH band ?EAST),
	?MAP(south,?SOUTH),
	?MAP(south_west,?SOUTH band ?WEST),
	?MAP(west,?WEST),
	?MAP(north_west, ?NORTH band ?WEST)
       ).

-endif.
