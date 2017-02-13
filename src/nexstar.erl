%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    NexStar communication protocol
%%% @end
%%% Created : 27 Oct 2014 by Tony Rogvall <tony@rogvall.se>

-module(nexstar).

-compile(export_all).

-export([open/1, close/1]).
-export([goto/2, goto/3, wait/1]).
-export([is_goto_in_progress/1]).
-export([cancel_goto/1]).
-export([sync/2]).
-export([is_alignment_complete/1]).
-export([get_tracking_mode/1]).
-export([set_tracking_mode/2]).
-export([get_position/1]).
-export([get_position/2]).
-export([set_time/1, set_time/2, set_time/3]).
-export([get_time/1]).
-export([set_location/2, get_location/1]).
-export([get_local_dst/0]).
-export([get_zone_offset/0]).

-export([gps_is_linked/1, gps_get_latitude/1, gps_get_longitude/1,
	 gps_get_date/1, gps_get_time/1]).
-export([rtc_get_date/1, rtc_get_time/1]).


-type uart() :: port().

-record(nexstar,
	{
	  uart :: undefined | uart(),
	  version = {1,2} :: {byte(),byte()},
	  model = -1 :: byte()
	}).

-type nexstar() :: #nexstar{}.


%% setup with  9600 baud
open(Dev) ->
    case uart:open(Dev, [{baud,9600},{stopb,1},{parity,none},
			 {mode,binary},{packet,0}]) of
	{ok,U} ->
	    ok = sync_(U),
	    {ok,<<Major,Minor>>} = get_version_(U),
	    {ok,<<Model>>} = get_model_(U),
	    {ok,#nexstar{uart=U, version={Major,Minor}, model=Model}};
	Error ->
	    Error
    end.

close(Nexstar) ->
    uart:close(Nexstar#nexstar.uart).

get_version_(Uart) when is_port(Uart) ->
    command_(Uart, <<$V>>, 2).

get_model_(Uart) when is_port(Uart) ->
    command_(Uart, <<$m>>, 1).

sync_(Uart) ->
    sync_(Uart, 10).

sync_(_Uart, 0) ->
    error;
sync_(Uart, I) ->
    case echo_(Uart, 16#55) of
	ok -> ok;
	_ -> sync_(Uart, I-1)
    end.

-spec echo_(uart(), byte()) -> ok | error.
echo_(Uart, X)  ->
    case command_(Uart, <<$K,X>>, 1) of
	{ok,<<X>>} -> ok;
	_ -> error
    end.


-spec goto(nexstar(), {float(),float()}) -> ok | error.

%% Goto using
%% azm/alt 
%% or 
%% ra_dec  Right ascension / Declination
%%

goto(Nexstar, Pos) ->
    goto(Nexstar, Pos, azm_alt).

-spec goto(nexstar(), {float(),float()}, azm_alt|ra_dec) -> ok | error.
goto(Nexstar, {AZM,ALT}, azm_alt) ->
    V = Nexstar#nexstar.version,
    if V >= {2,2} ->
	    Azm = frac_to_hex8(AZM),
	    Alt = frac_to_hex8(ALT),
	    command(Nexstar,<<$b,Azm/binary,$,,Alt/binary,$#>>,0);
       true ->
	    Azm = frac_to_hex4(AZM),
	    Alt = frac_to_hex4(ALT),
	    command(Nexstar,<<$B,Azm/binary,$,,Alt/binary,$#>>,0)
    end;
goto(Nexstar, {RA,DEC}, ra_dec) ->
    V = Nexstar#nexstar.version,
    if V >= {1,6} ->
	    Ra = frac_to_hex8(RA),
	    Dec = frac_to_hex8(DEC),
	    command(Nexstar,<<$r,Ra/binary,$,,Dec/binary,$#>>,0);
       true ->
	    Ra = frac_to_hex4(RA),
	    Dec = frac_to_hex4(DEC),
	    command(Nexstar,<<$R,Ra/binary,$,,Dec/binary,$#>>,0)
    end.

%% wait for goto to complete
wait(Nexstar) ->
    case is_goto_in_progress(Nexstar) of
	true ->
	    timer:sleep(1000),
	    wait(Nexstar);
	false ->
	    ok;
	error ->
	    error
    end.
    

sync(Nexstar, {RA,DEC}) ->
    V = Nexstar#nexstar.version,
    if V >= {4,10} ->
	    Ra = frac_to_hex8(RA),
	    Dec = frac_to_hex8(DEC),
	    command(Nexstar,<<$S,Ra/binary,$,,Dec/binary,$#>>,0);
       true ->  %% when to use?
	    Ra = frac_to_hex4(RA),
	    Dec = frac_to_hex4(DEC),
	    command(Nexstar,<<$s,Ra/binary,$,,Dec/binary,$#>>,0)
    end.

is_alignment_complete(Nexstar) ->
    case command(Nexstar, <<$J>>, 1) of
	{ok, <<1>>} -> true;
	{ok, <<0>>} -> false;
	_ -> error
    end.

is_goto_in_progress(Nexstar) ->
    case command(Nexstar, <<$L>>, 1) of
	{ok, <<$1>>} -> true;
	{ok, <<$0>>} -> false;
	_ -> error
    end.

cancel_goto(Nexstar) ->
    case command(Nexstar, <<$M>>, 0) of
	{ok, <<$1>>} -> true;
	{ok, <<$0>>} -> false;
	_ -> error
    end.

set_fixed_slew(Nexstar,azm,Rate) when is_integer(Rate), Rate >= 0, Rate < 10 ->
    case command(Nexstar, <<$P,2,16,36,Rate,0,0,0>>, 0) of
	{ok, <<>>} -> ok;
	_ -> error
    end;
set_fixed_slew(Nexstar,azm,Rate) when is_integer(Rate), Rate < 0, Rate > -10 ->
    case command(Nexstar, <<$P,2,16,37,(-Rate),0,0,0>>, 0) of
	{ok, <<>>} -> ok;
	_ -> error
    end;
set_fixed_slew(Nexstar,alt,Rate) when is_integer(Rate), Rate >= 0, Rate < 10 ->
    case command(Nexstar, <<$P,2,17,36,Rate,0,0,0>>, 0) of
	{ok, <<>>} -> ok;
	_ -> error
    end;
set_fixed_slew(Nexstar,alt,Rate) when is_integer(Rate), Rate < 0, Rate > -10 ->
    case command(Nexstar, <<$P,2,17,37,(-Rate),0,0,0>>, 0) of
	{ok, <<>>} -> ok;
	_ -> error
    end.

set_slew(Nexstar,azm,Rate) when is_number(Rate),Rate >= 0,Rate < 16384 ->
    R = trunc(Rate*4),
    case command(Nexstar,<<$P,3,16,6,R:16,0,0>>) of
	{ok, <<>>} -> ok;
	_ -> error
    end;
set_slew(Nexstar,azm,Rate) when is_number(Rate),Rate<0,Rate >= -16384 ->
    R = trunc(-Rate*4),
    case command(Nexstar,<<$P,3,16,7,R:16,0,0>>) of
	{ok, <<>>} -> ok;
	_ -> error
    end;

set_slew(Nexstar,alt,Rate) when is_number(Rate),Rate >= 0,Rate < 16384 ->
    R = trunc(Rate*4),
    case command(Nexstar,<<$P,3,17,6,R:16,0,0>>) of
	{ok, <<>>} -> ok;
	_ -> error
    end;
set_slew(Nexstar,alt,Rate) when is_number(Rate),Rate<0,Rate >= -16384 ->
    R = trunc(-Rate*4),
    case command(Nexstar,<<$P,3,17,7,R:16,0,0>>) of
	{ok, <<>>} -> ok;
	_ -> error
    end.


get_tracking_mode(Nexstar) ->
    case command(Nexstar, <<$t>>, 1) of
	{ok, <<Mode>>} -> {ok, Mode};
	Error -> Error
    end.

set_tracking_mode(Nexstar, Mode) ->
    command(Nexstar, <<$T,Mode>>, 0).


%% get non-aligned value
get_position(Nexstar) ->
    get_position(Nexstar, azm_alt).


get_position(Nexstar, ra_dec) ->
    V = Nexstar#nexstar.version,
    if V >= {1,6} ->
	    position(command_(Nexstar#nexstar.uart, <<$e>>, 17));
       true ->
	    position(command_(Nexstar#nexstar.uart, <<$E>>, 9))
    end;
get_position(Nexstar, azm_alt) ->
    V = Nexstar#nexstar.version,
    if V >= {2,2} ->
	    position(command_(Nexstar#nexstar.uart, <<$z>>, 17));
       true ->
	    position(command_(Nexstar#nexstar.uart, <<$Z>>, 9))
    end.


set_time(N) ->
    set_time(N,{date(),time()}).

set_time(N, DateTime) ->
    set_time(N, DateTime, undefined).
    
set_time(N, {{Year,Month,Day},{Hour,Minute,Second}}, Dst) ->
    W0 = get_zone_offset(),
    W = if W0 < 0 -> 256 - W0;
	   true -> W0
	end,
    X = if Dst =:= undefined -> get_local_dst();
	   Dst =:= true -> 1;
	   Dst =:= false -> 0
	end,
    case command(N, <<$H,Hour,Minute,Second,(Year-2000),Month,Day,W,X>>, 0) of
	{ok,<<>>} -> ok;
	_ -> error
    end.

get_time(N) ->
    {ok,<<Hour,Minute,Second,Y,Month,Day,_W,_X>>} = command(N, <<$h>>, 8),
    {{Y+2000,Month,Day},{Hour,Minute,Second}}.


%% Long={E,F,G,H}, Lat={A,B,C,D}
set_location(N, {{E,F,G,H},{A,B,C,D}}) ->
    case command(N, <<$W,A,B,C,D,E,F,G,H>>, 0) of
	{ok,<<>>} -> ok;
	_ -> error
    end.

get_location(N) ->
    {ok,<<A,B,C,D,E,F,G,H>>} = command(N, <<$w>>, 8),
    {{E,F,G,H},{A,B,C,D}}.

get_local_dst() ->
    L = erlang:localtime(),
    L0 = erlang:localtime_to_universaltime(L),
    L1 = erlang:localtime_to_universaltime(L, true),
    if L0 =:= L1 -> 1;
       true -> 0
    end.

%% why only integer numbers?
get_zone_offset() ->
    L = erlang:localtime(),
    G = erlang:localtime_to_universaltime(L),
    LG = calendar:datetime_to_gregorian_seconds(L),
    GG = calendar:datetime_to_gregorian_seconds(G),
    (LG - GG) div 3600.

%% GPS commands
-spec gps_is_linked(N::nexstar()) -> boolean() | error.
gps_is_linked(N) ->
    if N#nexstar.version >= {1,6} ->
	    {ok,<<X>>} = command(N, <<$P,1,176,55,0,0,0,1>>, 1),
	    X > 0;
       true ->
	    error
    end.

gps_get_latitude(N) ->
    if N#nexstar.version >= {1,6} ->
	    {ok,<<X,Y,Z>>} = command(N, <<$P,1,176,1,0,0,0,3>>, 3),
	    {ok, (X*65536+Y*256+Z) / (1 bsl 24)};
       true ->
	    error
    end.

gps_get_longitude(N) ->
    if N#nexstar.version >= {1,6} ->
	    {ok,<<X,Y,Z>>} = command(N, <<$P,1,176,2,0,0,0,3>>, 3),
	    {ok, (X*65536+Y*256+Z) / (1 bsl 24)};
       true ->
	    error
    end.

gps_get_date(N) ->
    {ok,{Month,Day}} = gps_get_date_(N),
    {ok,Year} = gps_get_year_(N),
    {ok,{Year,Month,Day}}.
    

gps_get_date_(N) ->
    if N#nexstar.version >= {1,6} ->
	    {ok,<<X,Y>>} = command(N, <<$P,1,176,3,0,0,0,2>>, 2),
	    {ok, {X,Y}};
       true ->
	    error
    end.

gps_get_year_(N) ->
    if N#nexstar.version >= {1,6} ->
	    {ok,<<X,Y>>} = command(N, <<$P,1,176,4,0,0,0,2>>, 2),
	    {ok, X*256+Y};
       true ->
	    error
    end.

gps_get_time(N) ->
    if N#nexstar.version >= {1,6} ->
	    {ok,<<X,Y,Z>>} = command(N, <<$P,1,176,51,0,0,0,3>>, 3),
	    {ok, {X,Y,Z}};
       true ->
	    error
    end.


%% RTC commands

%% {Year,Month,Day}
rtc_get_date(N) ->
    {ok,<<Month,Day>>} = rtc_get_date_(N),
    {ok,<<Y1,Y2>>} = rtc_get_year_(N),
    {ok, {Y1*256+Y2, Month, Day}}.

%% {Hour,Minute,Second}
rtc_get_time(N) ->
    {ok,<<X,Y,Z>>} = rtc_get_time_(N),
    {ok, {X,Y,Z}}.

-spec rtc_get_date_(nexstar()) -> {ok,binary()} | error.
%% {ok,<<X,Y>>}
%% X is month 1-12, Y is day 1-31
rtc_get_date_(Nexstar) ->
    command_(Nexstar#nexstar.uart, <<$P,1,178,3,0,0,0,2>>, 2).

-spec rtc_get_year_(nexstar()) -> {ok,binary()} | error.
%% Year = X*256 + Y
rtc_get_year_(Nexstar) ->
    command_(Nexstar#nexstar.uart, <<$P,1,178,4,0,0,0,2>>, 2).

-spec rtc_get_time_(nexstar()) -> {ok,binary()} | error.
%% {ok,<<X,Y,Z>>} where X is hour, Y is minutes, Z is seconds
rtc_get_time_(Nexstar) ->
    command_(Nexstar#nexstar.uart, <<$P,1,178,51,0,0,0,3>>, 3).


command(Nexstar, Command) ->
    command_(Nexstar#nexstar.uart, Command, 0).

command(Nexstar, Command, Collect) ->
    command_(Nexstar#nexstar.uart, Command, Collect).
    
command_(Uart, Command, Collect) ->
    io:format("command: ~p\n", [Command]),
    uart:send(Uart, Command),
    collect_(Uart, Collect).


collect_(Uart, 0) ->
    collect_hash_(Uart, []);
collect_(Uart, Len) ->
    case uart:recv(Uart, Len+1, 4000) of
	{ok, Data0} ->
	    io:format("data: ~p\n", [Data0]),
	    case Data0 of
		<<Data:Len/binary,$#>> ->
		    {ok,Data};
		<<Data:Len/binary,_>> ->
		    collect_hash_(Uart,[]), %% sync
		    {ok,Data}
	    end;
	Error ->
	    Error
    end.

collect_hash_(Uart, Acc) ->
    case uart:recv(Uart, 1, 4000) of
	{ok, <<$#>>} ->
	    {ok, erlang:iolist_to_binary(lists:reverse(Acc))};
	{ok, <<Byte>>} ->
	    collect_hash_(Uart, [Byte|Acc]);
	Error ->
	    Error
    end.

%% convert a float value [0..1] -> hex
frac_to_hex8(Value) when Value >= 0.0, Value =< 1.0 ->
    Val0 = trunc(Value*16#100000000) + 16#100000000,
    <<$1,Val:8/binary>> = erlang:integer_to_binary(Val0,16),
    Val.

%% convert a float value [0..1] -> hex
frac_to_hex4(Value) when Value >= 0.0, Value =< 1.0 ->
    Val0 = trunc(Value*16#10000) + 16#10000,
    <<$1,Val:4/binary>> = erlang:integer_to_binary(Val0,16),
    Val.
    

position({ok,<<V1:8/binary, $,, V2:8/binary>>}) ->
    {ok, {binary_to_integer(V1, 16)/16#100000000,
	  binary_to_integer(V2, 16)/16#100000000}};
position({ok,<<V1:4/binary, $,, V2:4/binary>>}) ->
    {ok,{binary_to_integer(V1, 16) / 16#10000, 
	 binary_to_integer(V2, 16)/ 16#10000 }};
position(error) ->
    error.
