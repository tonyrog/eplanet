%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Use epx to view planets in orbit
%%% @end
%%% Created : 25 Jan 2015 by Tony Rogvall <tony@rogvall.se>

-module(eplanet_view).

-export([start/0, start/3]).

-define(SECONDS_PER_DAY, (60*60*24)).
-define(DEFAULT_SCALE, 50).

-include_lib("epx/include/epx.hrl").
-include_lib("epx/include/epx_image.hrl").

-record(s,
	{
	  window,
	  background,
	  pixmap  :: #epx_pixmap{},
	  planets :: #{ atom() => #epx_pixmap{} },
	  longitude = 0,
	  latitude = 0,
	  tz_hour_offset = 0.0,
	  font    :: #epx_font{},
	  width,
	  height,
	  scale = ?DEFAULT_SCALE,
	  center = sun,
	  days_per_tick = 1/48,   %% one half hour per tick
	  ticks_per_seconds = 1,  %% one tick per seconds
	  tmo = 100,              %% tick timeout
	  pause = false
	}).

-export([load_planets/0]).


start() ->
    DateTime = {date(), time()},
    start(1024, 576, DateTime).

start(Width, Height, DateTime) ->
    application:ensure_all_started(epx),
    spawn(fun() -> draw_init_(Width, Height, DateTime) end).

draw_init_(Width, Height, DateTime) ->
    Pixmap = epx:pixmap_create(Width,Height),
    Bg = epx:pixmap_create(Width,Height),
    epx:pixmap_fill(Bg, black),

    epx:pixmap_attach(Bg),
    Win = epx:window_create(50, 50, Width, Height,
                            [button_press, button_release,
			     key_press]),
    epx:window_attach(Win),

    {ok,Font} = epx_font:match([{name,"Arial"},{size,12}]),
    Day = eplanet:j2000_days(DateTime),
    Planets = load_planets(),
    {Longitude,Latitude,TZ_Hour_Offset} = eplanet:location(),
    S0 = #s{ window = Win,
	     background = Bg,
	     pixmap = Pixmap,
	     planets = Planets,
	     longitude = Longitude,
	     latitude = Latitude,
	     tz_hour_offset = TZ_Hour_Offset,
	     font = Font,
	     width = Width,
	     height = Height },
    S1 = timeout(S0),
    epx_gc:set_font(Font),
    %% epx_view:scale(Pixmap, 1, -1), %% flip to "normal" coordinates
    epx_view:translate(Pixmap, Width/2, Height/2),
    epx_view:scale(Pixmap, S1#s.scale, S1#s.scale),
    draw_loop(Win, Day, S1).

draw_loop(Win, Day, S) ->
    draw(Day, S),
    T0 = os:timestamp(),
    receive
	{epx_event, Win, close} ->
	    epx:window_detach(S#s.window),
	    epx:pixmap_detach(S#s.background),
	    ok;
	{epx_event, Win, {key_press,$q,_,_}} ->
	    epx:window_detach(S#s.window),
	    epx:pixmap_detach(S#s.background),
	    ok;
	{epx_event, Win, {key_press,$e,_,_}} ->
	    draw_loop(Win, Day, S#s { center = earth});
	{epx_event, Win, {key_press,$s,_,_}} ->
	    draw_loop(Win, Day, S#s { center = sun});
	{epx_event, Win, {key_press,$\s,_,_}} ->
	    S1 = timeout(S#s { pause = not S#s.pause }),
	    draw_loop(Win, Day, S1);
	{epx_event, Win, {key_press,$+,_,_}} ->
	    epx_view:scale(S#s.pixmap, 2, 2),
	    S1 = S#s { scale = S#s.scale * 2 },
	    draw_loop(Win, Day, S1);
	{epx_event, Win, {key_press,$-,_,_}} ->
	    epx_view:scale(S#s.pixmap, 0.5, 0.5),
	    S1 = S#s { scale = S#s.scale/2 },
	    draw_loop(Win, Day, S1);
	{epx_event, Win, {key_press,right,_,_}} when S#s.pause ->
	    draw_loop(Win, Day+S#s.days_per_tick, S);
	{epx_event, Win, {key_press,left,_,_}} when S#s.pause ->
	    draw_loop(Win, Day-S#s.days_per_tick, S);
	{epx_event, Win, {key_press,up,_,_}} ->
	    TicksPerSeconds = S#s.ticks_per_seconds + 1,
	    S1 = timeout(S#s { ticks_per_seconds = TicksPerSeconds }),
	    draw_loop(Win, Day, S1);
	{epx_event, Win, {key_press,down,_,_}} ->
	    TicksPerSeconds = max(1, S#s.ticks_per_seconds - 1),
	    S1 = timeout(S#s { ticks_per_seconds = TicksPerSeconds }),
	    draw_loop(Win, Day, S1);
	{epx_event, Win, {key_press,$D,_,_}} ->
	    S1 = S#s { days_per_tick = 1 },
	    draw_loop(Win, Day, S1);
	{epx_event, Win, {key_press,$H,_,_}} ->
	    S1 = S#s { days_per_tick = 1/48 },
	    draw_loop(Win, Day, S1);
	Other ->
	    io:format("got: ~p\n", [Other]),
	    draw_loop(Win, Day, S)
    after S#s.tmo ->
	    T1 = os:timestamp(),
	    Td = timer:now_diff(T1, T0) / 1000000,
	    Ticks = Td*S#s.ticks_per_seconds,
	    Day1 = Day + Ticks*S#s.days_per_tick,
	    draw_loop(Win, Day1, S)
    end.

%% calculate the new timeout value given hours_per_seconds
timeout(S) ->
    if S#s.pause -> 
	    S#s { tmo = infinity };
       true ->
	    S#s { tmo = max(10, trunc(1000*(1/S#s.ticks_per_seconds))) }
    end.

draw(Day, S) ->
    epx:pixmap_fill(S#s.pixmap, black),
    draw_date(S#s.pixmap, Day),
    draw_planets(S#s.pixmap, Day, S),
    epx:pixmap_copy_to(S#s.pixmap, S#s.background),
    epx:pixmap_draw(S#s.background,S#s.window,0,0,0,0,S#s.width,S#s.height).

draw_planets(Pixmap, Day, S) ->
    draw_center(Pixmap, Day, S),
    epx_gc:set_fill_style(solid),
    Planets = case S#s.center of
		  earth -> [mercury, venus, sun, mars, jupiter,
			    saturn, uranus, neptune, pluto ];
		  sun ->
		      [mercury, venus, earth, mars, jupiter,
		       saturn, uranus, neptune, pluto ]
	      end,
    lists:foreach(fun(Body) -> draw_planet(Pixmap, Body, Day, S) end, Planets).


draw_date(Pixmap, D) ->
    {{YYYY,MM,DD},{H,M,S}} = eplanet:j2000_to_datetime(D),
    String = io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
			   [YYYY,MM,DD, H,M,S]),
    DateString = lists:flatten(String),
    epx_gc:set_foreground_color({0,255,255,255}),
    epx:draw_string(Pixmap, 10, 20, DateString).

draw_center(Pixmap, Day, S) ->
    R = draw_planet_(Pixmap, S#s.center, 0, 0, S),
%%    Color = color(S#s.center),
%%    R = radius(S#s.center)/S#s.scale,
%%    epx_gc:set_fill_color(Color),
%%    X = -R,
%%    Y = -R,
%%    epx_view:draw_ellipse(Pixmap, X, Y, 2*R, 2*R),
    draw_time_angle(S#s.center, Pixmap, 0, 0, R, Day, S).

%% try draw a time at our position
draw_time_angle(earth, Pixmap, X, Y, R, Day, S) ->
    HA = eplanet:local_sidereal_time(Day, S#s.longitude),
    epx_view:moveto(Pixmap, X-R, Y-R),
    epx_view:turnto(Pixmap, HA+180),
    epx_view:move(Pixmap, R),
    epx_gc:set_foreground_color(white),
    epx_view:line(Pixmap, 2);
draw_time_angle(_, _Pixmap, _X, _Y, _R, _Day,_S) -> 
    ok.
	    
draw_planet(Pixmap, Body, Day, S) ->
    {X0,Y0,_Z0} = 
	case S#s.center of
	    sun -> eplanet:position_ecl(Body, Day);
	    earth -> eplanet:position_geo(Body, Day)
	end,
    %% R=draw_ellipse_planet(Pixmap, Body, X0, Y0, S),
    R = draw_planet_(Pixmap, Body, X0, Y0, S),
    draw_time_angle(Body, Pixmap, X0, Y0, R, Day, S).


draw_planet_(Pixmap, Body, X0, Y0, S) ->
    Planet = maps:get(Body, S#s.planets),
    Width = epx:pixmap_info(Planet, width),
    Height = epx:pixmap_info(Planet, height),
    {X,Y} = epx_view:moveto(Pixmap,X0,Y0),
    R = radius(Body)/S#s.scale,
    Width1  = Width*radius(Body)*(S#s.scale/5000),
    Height1 = Height*radius(Body)*(S#s.scale/5000),
    X1 = X - (Width1/2),
    Y1 = Y - (Height1/2),
    epx:pixmap_scale_area(Planet, Pixmap,
			  trunc(X1),trunc(Y1),
			  trunc(Width1),
			  trunc(Height1), [blend]),
    R.

draw_planet__(Pixmap, Body, X0, Y0, S) ->
    Color = color(Body),
    R = radius(Body)/S#s.scale,
    X = X0 - R,
    Y = Y0 - R,
    epx_gc:set_fill_color(Color),
    epx_view:draw_ellipse(Pixmap, X, Y, 2*R, 2*R),
    R.


radius(sun) -> 10;
radius(moon) -> 1;
radius(mercury) -> 3;
radius(venus) -> 4;
radius(earth) -> 4;
radius(mars) -> 3;
radius(jupiter) -> 8;
radius(saturn) ->  7;
radius(uranus) -> 7;
radius(neptune) -> 6;
radius(pluto) -> 3.

color(sun) -> yellow;
color(moon) -> gray;
color(mercury) -> mediumvioletred;
color(venus) -> lightblue;
color(earth) -> cyan;
color(mars) -> red;
color(jupiter) -> orange;
color(saturn) ->  orange;
color(uranus) -> green;
color(neptune) -> blue;
color(pluto) -> white.

load_planets() ->
    maps:from_list(
      [{Planet,
	load_pixmap(
	  filename:join([code:priv_dir(eplanet), "images", File]))}
       || {Planet,File} <- [
			    {sun,"sun-transparent.png"},
			    {moon,"moon-transparent.png"},
			    {mercury,"mercury-transparent.png"},
			    {venus,"venus-transparent.png"},
			    {earth,"earth-transparent.png"},
			    {mars,"mars-transparent.png"},
			    {jupiter,"jupiter-transparent.png"},
			    {saturn,"saturn-transparent.png"},
			    {uranus,"uranus-transparent.png"},
			    {neptune,"neptune-transparent.png"},
			    {pluto,"pluto.png"}
			   ]]).

load_pixmap(File) ->
    case epx_image:load(File) of
	{ok, #epx_image { pixmaps = [Pixmap]}} ->
	    Pixmap
    end.
			 
	    
