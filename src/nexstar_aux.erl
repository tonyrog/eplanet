%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    NexStar Aux port commands
%%% @end
%%% Created :  1 Jan 2019 by Tony Rogvall <tony@rogvall.se>

-module(nexstar_aux).
-compile(export_all).

-include("nexstar.hrl").

%% max message size = 255 - 3 = 252
-record(message,
	{
	 preamable = 16#3b,   %% :8
	 packet_len,          %% :8  count source,dest,message_id,message_data
	 source_device,       %% :8
	 destination_device,  %% :8
	 message_id,          %% :8
	 message_data=(<<>>), %% :N/binary
	 checksum             %% :8  include packet_len .. message_data
	}).

%% Motor controller messages

-define(MC_GET_POSITION,      16#01).
-define(MC_GOTO_FAST,         16#02).
-define(MC_SET_POSITION,      16#04).
-define(MC_SET_POS_GUIDERATE, 16#06).
-define(MC_SET_NEG_GUIDERATE, 16#07).
-define(MC_LEVEL_START,       16#0b).
-define(MC_PEC_RECORD_START,  16#0c).
-define(MC_PEC_PLAYBACK,      16#0d).
-define(MC_SET_POS_BACKLASH,  16#10).
-define(MC_SET_NEG_BACKLASH,  16#11).
-define(MC_LEVEL_DONE,        16#12).
-define(MC_SLEW_DONE,         16#13).
-define(MC_UNKNOWN1,          16#14).
-define(MC_PEC_RECORD_DONE,   16#15).
-define(MC_PEC_RECORD_STOP,   16#16).
-define(MC_GOTO_SLOW,         16#17).
-define(MC_AT_INDEX,          16#18).
-define(MC_SEEK_INDEX,        16#19).
-define(MC_MOVE_POS,          16#24).
-define(MC_MOVE_NEG,          16#25).
-define(MC_ENABLE_CORDWRAP,   16#38).
-define(MC_DISABLE_CORDWRAP,  16#39).
-define(MC_SET_CORDWRAP_POS,  16#3a).
-define(MC_POLL_CORDWRAP,     16#3b).
-define(MC_GET_CORDWRAP_POS,  16#3c).
-define(MC_GET_POS_BACKLASH,  16#40).
-define(MC_GET_NEG_BACKLASH,  16#41).
-define(MC_SET_AUTOGUIDE_RATE, 16#46).
-define(MC_GET_AUTOGUIDE_RATE, 16#47).
-define(MC_PROGRAM_ENTER,      16#81).
-define(MC_PROGRAM_INIT,       16#82).
-define(MC_PROGRAM_DATA,       16#83).
-define(MC_PROGRAM_END,        16#84).
-define(MC_GET_APPROACH,       16#fc).
-define(MC_SET_APPROACH,       16#fd).
-define(MC_GET_VER,            16#fe).

-define(MAIN_GET_VER,          16#fe).

-define(GPS_GET_LAT,         16#01).
-define(GPS_GET_LONG,        16#02).
-define(GPS_GET_DATE,        16#03).
-define(GPS_GET_YEAR,        16#04).
-define(GPS_GET_SAT_INFO,    16#07).
-define(GPS_GET_RCVR_STATUS, 16#08).
-define(GPS_GET_TIME,        16#33).
-define(GPS_TIME_VALID,      16#36).
-define(GPS_LINKED,          16#37).
-define(GPS_GET_HW_VER,      16#55).
-define(GPS_GET_COMPASS,     16#a0).
-define(GPS_GET_VER,         16#fe).

-define(SRC, app).

%% MC - Motor controller
mc_get_ver(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_GET_VER)) of
	{ok,<<MSB,LSB>>} -> {ok,{MSB,LSB}};
	{ok,Val} -> {ok,{badval,Val}};
	Error -> Error
    end.

mc_get_position(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_GET_POSITION)) of
	{ok,<<Fraction:24/signed>>} -> {ok,fraction_to_dms(Fraction)};
	Error -> Error
    end.

mc_goto_fast(Aux,Dev,Pos)  -> %% select 16/24 resolution?
    Fraction = pos_to_fraction(Pos),
    command(Aux, make_message(?SRC,Dev,?MC_GOTO_FAST, <<Fraction:24>>)).

mc_goto_slow(Aux,Dev,Pos)  -> %% select 16/24 resolution?
    Fraction = pos_to_fraction(Pos),
    command(Aux, make_message(?SRC,Dev,?MC_GOTO_SLOW, <<Fraction:24>>)).

mc_set_poistion(Aux,Dev,Pos)  ->
    Fraction = pos_to_fraction(Pos),
    command(Aux, make_message(?SRC,Dev,?MC_SET_POSITION, <<Fraction:24>>)).

mc_set_pos_guidrate(Aux,Dev,sidereal) ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_POS_GUIDERATE, <<16#ffff:16>>));
mc_set_pos_guidrate(Aux,Dev,solar) ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_POS_GUIDERATE, <<16#fffe:16>>));
mc_set_pos_guidrate(Aux,Dev,lunar) ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_POS_GUIDERATE, <<16#fffd:16>>));
mc_set_pos_guidrate(Aux,Dev,Rate) when is_integer(Rate) ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_POS_GUIDERATE, <<Rate:24>>)).

mc_set_neg_guidrate(Aux,Dev,sidereal) ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_NEG_GUIDERATE, <<16#ffff:16>>));
mc_set_neg_guidrate(Aux,Dev,solar) ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_NEG_GUIDERATE, <<16#fffe:16>>));
mc_set_neg_guidrate(Aux,Dev,lunar) ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_NEG_GUIDERATE, <<16#fffd:16>>));
mc_set_neg_guidrate(Aux,Dev,Rate) when is_integer(Rate) ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_NEG_GUIDERATE, <<Rate:24>>)).

mc_level_start(Aux) ->
    command(Aux, make_message(?SRC,alt,?MC_LEVEL_START, <<>>)).

mc_level_done(Aux) ->
    case command(Aux, make_message(?SRC,alt,?MC_LEVEL_DONE,<<>>)) of
	{ok,<<0>>}     -> {ok,false};
	{ok,<<16#ff>>} -> {ok,true};
	{ok,Val} -> {error,{badval,Val}};
	Error -> Error
    end.

mc_pec_record_start(Aux,Dev) ->    
    command(Aux, make_message(?SRC,Dev,?MC_PEC_RECORD_START, <<>>)).

mc_pec_record_stop(Aux,Dev) ->    
    command(Aux, make_message(?SRC,Dev,?MC_PEC_RECORD_STOP, <<>>)).

mc_pec_record_done(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_PEC_RECORD_DONE,<<>>)) of
	{ok,<<0>>}     -> {ok,false};
	{ok,<<16#ff>>} -> {ok,true};
	{ok,Val} -> {error,{badval,Val}};
	Error -> Error
    end.

mc_pec_playback(Aux,Dev,start) -> mc_pec_playback(Aux,Dev,1);
mc_pec_playback(Aux,Dev,stop) -> mc_pec_playback(Aux,Dev,0);
mc_pec_playback(Aux,Dev,Cmd) when is_integer(Cmd) ->    
    command(Aux, make_message(?SRC,Dev,?MC_PEC_PLAYBACK, <<Cmd>>)).

mc_set_pos_backlash(Aux,Dev,Val) when is_integer(Val), Val >= 0, Val =< 99 ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_POS_BACKLASH, <<Val>>)).    

mc_set_neg_backlash(Aux,Dev,Val) when is_integer(Val), Val >= 0, Val =< 99 ->
    command(Aux, make_message(?SRC,Dev,?MC_SET_NEG_BACKLASH, <<Val>>)).

mc_slew_done(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_SLEW_DONE,<<>>)) of
	{ok,<<0>>} -> {ok,false};
	{ok,<<16#ff>>} -> {ok,true};
	{ok,Val} -> {error,{badval,Val}};
	Error -> Error
    end.

mc_at_index(Aux) ->
    case command(Aux, make_message(?SRC,azm,?MC_AT_INDEX,<<>>)) of
	{ok,<<0>>} -> {ok,false};
	{ok,<<16#ff>>} -> {ok,true};
	{ok,Val} -> {error,{badval,Val}};
	Error -> Error
    end.

mc_seek_index(Aux) ->
    case command(Aux, make_message(?SRC,azm,?MC_SEEK_INDEX,<<>>)) of
	{ok,_} -> ok;
	Error -> Error
    end.

mc_move_pos(Aux, Dev, Value) when is_integer(Value), Value >= 0, Value =< 9 ->
    command(Aux, make_message(?SRC,Dev,?MC_MOVE_POS,<<Value>>)).

mc_move_neg(Aux, Dev, Value) when is_integer(Value), Value >= 0, Value =< 9 ->
    command(Aux, make_message(?SRC,Dev,?MC_MOVE_NEG,<<Value>>)).

mc_enable_cordwrap(Aux) ->
    command(Aux, make_message(?SRC,azm,?MC_ENABLE_CORDWRAP)).

mc_disable_cordwrap(Aux) ->
    command(Aux, make_message(?SRC,azm,?MC_DISABLE_CORDWRAP)).

mc_set_cordwrap_pos(Aux,Pos) ->
    Fraction = pos_to_fraction(Pos),
    command(Aux, make_message(?SRC,azm,?MC_GET_CORDWRAP_POS, <<Fraction:24>>)).

mc_poll_cordwrap(Aux) ->
    case command(Aux, make_message(?SRC,azm,?MC_GET_CORDWRAP_POS)) of
	{ok,<<0>>} -> {ok,disabled};
	{ok,<<255>>} -> {ok,enabled};
	{ok,Val} -> {error,{badval,Val}};
	Error -> Error
    end.

mc_get_cordwrap_pos(Aux) ->
    command(Aux, make_message(?SRC,azm,?MC_GET_CORDWRAP_POS)).

mv_get_pos_backlash(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_GET_POS_BACKLASH)) of
	{ok,<<Val>>} -> {ok,Val};
	Error -> Error
    end.

mv_get_neg_backlash(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_GET_NEG_BACKLASH)) of
	{ok,<<Val>>} -> {ok,Val};
	Error -> Error
    end.

mc_get_autoguide_rate(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_GET_AUTOGUIDE_RATE)) of
	{ok,<<Val>>} -> {ok,100*Val/256};
	Error -> Error
    end.

mc_program_enter(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_PROGRAM_ENTER)) of
	{ok,<<Val>>} -> {ok,Val};
	Error -> Error
    end.

mc_program_init(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_PROGRAM_INIT)) of
	{ok,<<Val>>} -> {ok,Val};
	Error -> Error
    end.

mc_program_data(Aux,Dev,Data) -> %% 2+2*N bytes
    case command(Aux, make_message(?SRC,Dev,?MC_PROGRAM_DATA, Data)) of
	{ok,<<Val>>} -> {ok,Val};
	Error -> Error
    end.

mc_program_end(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_PROGRAM_END)) of
	{ok,<<Val>>} -> {ok,Val};
	Error -> Error
    end.

mc_get_approach(Aux,Dev) ->
    case command(Aux, make_message(?SRC,Dev,?MC_GET_APPROACH)) of
	{ok,<<0>>} -> {ok,positive};
	{ok,<<1>>} -> {ok,negative};
	{ok,Val} -> {error,{badval,Val}};
	Error -> Error
    end.

mc_set_approach(Aux,Dev,Val) when Val =:= 0; Val =:= 1 ->
    case command(Aux, make_message(?SRC,Dev,?MC_SET_APPROACH,<<Val>>)) of
	{ok,_} -> ok;
	Error -> Error
    end.

%% MAIN
main_get_ver(Aux) ->
    case command(Aux, make_message(?SRC,main,?MAIN_GET_VER)) of
	{ok,<<MSB,LSB>>} -> {ok,{MSB,LSB}};
	{ok,Val} -> {error,{badval,Val}};
	Error -> Error
    end.
	    
	    

fraction_to_dms(F) ->
    Sign = if F < 0 -> -1; true -> 1 end,
    D = 360*(abs(F)/(1 bsl 24)),
    DD = trunc(D),
    MM = trunc((D-DD)*60),
    SS = (D-DD-MM/60)*3600,
    {Sign*DD,MM,SS}.

dms_to_decimal({D,M,S}) when D < 0 ->
    -(abs(D) + (M/60) + (S/3600));
dms_to_decimal({D,M,S}) ->
    D + (M/60) + (S/3600).

pos_to_fraction(Pos) when is_integer(Pos) ->
    Pos;
pos_to_fraction(DMS) when is_tuple(DMS), tuple_size(DMS) =:= 3 ->
    dms_to_fraction(DMS).

dms_to_fraction(DMS) ->
    Decimal = dms_to_decimal(DMS),
    trunc((Decimal/360) * (1 bsl 24)).
    
%% GPS
gps_get_lat(Aux) ->
    case command(Aux, make_message(?SRC,gps,?GPS_GET_LAT)) of
	{ok,<<Lat:24/signed>>} -> {ok,fraction_to_dms(Lat)};
	Error -> Error
    end.

gps_get_long(Aux) ->
    case command(Aux, make_message(?SRC,gps,?GPS_GET_LONG)) of
	{ok,<<Long:24/signed>>} -> {ok,fraction_to_dms(Long)};
	Error -> Error
    end.

gps_get_date(Aux) ->
    case command(Aux, make_message(app,gps,?GPS_GET_DATE)) of
	{ok,<<Month,Day>>} -> {ok,{Month,Day}};
	Error -> Error
    end.

gps_get_year(Aux) ->
    case command(Aux, make_message(app,gps,?GPS_GET_YEAR)) of
	{ok,<<Year:16>>} -> {ok,Year};
	Error -> Error
    end.

gps_get_sat_info(Aux) ->
    case command(Aux, make_message(app,gps,?GPS_GET_SAT_INFO)) of
	{ok,<<Visible,Tracked>>} -> {ok,{Visible, Tracked}};
	Error -> Error
    end.

gps_get_rcvr_status(Aux) ->
    command(Aux, make_message(app,gps,?GPS_GET_RCVR_STATUS)).

gps_get_time(Aux) ->
    case command(Aux, make_message(app,gps,?GPS_GET_TIME)) of
	{ok,<<Hour,Minute,Second>>} -> {ok,{Hour,Minute,Second}};
	Error -> Error
    end.

gps_time_valid(Aux) ->
    case command(Aux, make_message(app,gps,?GPS_TIME_VALID)) of
	{ok,<<0>>} -> {ok,false};
	{ok,_} -> {ok, true};
	Error -> Error
    end.
	    
gps_linked(Aux) ->
    case command(Aux, make_message(app,gps,?GPS_LINKED)) of
	{ok,<<0>>} -> {ok,false};
	{ok,_} -> {ok,true};
	Error -> Error
    end.

gps_get_hw_ver(Aux) ->
    command(Aux, make_message(app,gps,?GPS_GET_HW_VER)).

-define(MAP(K,V), V => K).
-define(rev_compass_map, #{ ?COMPASS_MAP }).
gps_get_compass(Aux) ->
    case command(Aux, make_message(app,gps,?GPS_GET_COMPASS)) of
	{ok,<<C>>} ->
	    case maps:find(C, ?rev_compass_map) of
		{ok, Dir} -> Dir;
		error -> {ok, unknown}
	    end;
	Error -> Error
    end.
-undef(MAP).


gps_get_ver(Aux) ->
    case command(Aux, make_message(app,gps,?GPS_GET_VER)) of
	{ok,<<Maj,Min>>} -> {ok,{Maj,Min}};
	Error -> Error
    end.
	    

gps_info(Aux) ->
    {ok,Linked} = gps_linked(Aux),
    {ok,{Visible,Tracked}} = gps_get_sat_info(Aux),
    {ok,{H,M,S}} = gps_get_time(Aux),
    {ok,{Month,Day}} = gps_get_date(Aux),
    {ok,Year} = gps_get_year(Aux),
    {ok,Long} = gps_get_long(Aux),
    {ok,Lat} = gps_get_lat(Aux),
    [{linked,Linked}, {visible,Visible}, {tracked, Tracked},
     {time,{H,M,S}}, {date,{Year,Month,Day}}, {long,Long}, {lat,Lat}].
     
open() ->
    case find_interface() of
	{_IfAddr,_Router,_Mask} ->
	    Options = [{mode,binary},{packet,0},{active,0}],
	    gen_tcp:connect("1.2.3.4", 2000, Options);
	Error ->
	    {error,Error}
    end.

run_udp() ->
    case find_interface() of
	{IfAddr,_Router,_Mask} ->
	    spawn(fun() ->
			  Options = [{ip,IfAddr},{mode,binary},{active,true}],
			  case gen_udp:open(55555, Options) of
			      {ok,U} ->
				  loop_udp(U);
			      Error ->
				  io:format("unable to listen to 55555 ~p\n", 
					    [Error])
			  end
		  end);
	Error ->
	    io:format("interface not ready ~p\n", [Error])
    end.

loop_udp(U) ->
    receive
	{udp, U, IP, Port, Data} ->
	    io:format("got data from ~p:~w ~p\n", [IP,Port,Data]),
	    loop_udp(U);
	Other ->
	    io:format("Got: other = ~p\n", [Other]),
	    loop_udp(U)
    end.

find_interface() ->
    find_interface(inet:getif()).

find_interface({ok,IFList}) ->
    find_interface_(IFList);
find_interface(Error={error,_}) ->
    Error.

find_interface_([IF={{1,2,3,_},_,_}|_]) ->
    IF;
find_interface_([_|IFList]) ->
    find_interface_(IFList);
find_interface_([]) ->
    false.

close(Aux) ->
    gen_tcp:close(Aux).

command(Aux, Message) ->
    Message1 = set_checksum(Message),
    print_message(Message1),
    ok = gen_tcp:send(Aux, message_list(Message1)),
%%    flush_message(Aux).
    {ok,Echo} = poll_message(Aux),  %% check echo
    print_message(Echo),
    {ok,Reply} = poll_message(Aux),  %% check reply!
    print_message(Reply),
    {ok,Reply#message.message_data}.

flush_message(Aux) ->
    inet:setopts(Aux, [{active, once}]),
    receive
	{tcp_passive, Aux} ->
	    io:format("flush passive\n"),
	    flush_message(Aux);	    
	{tcp, Aux, Data} ->
	    io:format("data: ~p\n", [Data]),
	    flush_message(Aux)
    after 2000 ->
	    inet:setopts(Aux, [{active, false}]),
	    ok
    end.

poll_message(Aux) ->
    case gen_tcp:recv(Aux, 5) of
	{ok,<<16#3b,MessageHeader:4/binary>>} ->
	    <<PacketLen,Src,Dst,MessageID>> = MessageHeader,
	    io:format("message header = ~p\n", [MessageHeader]),
	    MessageLen = PacketLen-3,
	    io:format("message len = ~p\n", [MessageLen]),	    
	    case gen_tcp:recv(Aux, MessageLen+1) of
		{ok,<<MessageData:MessageLen/binary, Checksum>>} ->
		    io:format("message data=~p, checksum=~p\n", 
			      [MessageData, Checksum]),
		    Sum = sum_bytes([MessageHeader,MessageData],0),
		    case -Sum band 16#ff of
			Checksum ->
			    {ok,make_message(Src,Dst,MessageID,MessageData,
					     Checksum)};
			_ ->
			    {error,bad_checksum}
		    end;
		Error ->
		    Error
	    end;
	Error -> Error
    end.

set_checksum(Message) when is_record(Message,message) ->
    [_,16#3b|Data] = tuple_to_list(Message),
    Sum = sum_bytes(Data,0),
    Checksum = (-Sum) band 16#ff,
    Message#message{ checksum = Checksum }.

sum_bytes([Data|Ds],Sum) when is_binary(Data) ->
    sum_bytes(Ds, sum_binary(Data,Sum));
sum_bytes([Data|Ds],Sum) when is_list(Data) ->
    sum_bytes(Ds, sum_bytes(Data,Sum));
sum_bytes([Data|Ds],Sum) when is_integer(Data) ->
    sum_bytes(Ds,Data+Sum);
sum_bytes([undefined],Sum) -> Sum; %% checksum field
sum_bytes([],Sum) -> Sum.

sum_binary(<<C,Bin/binary>>, Sum) ->    
    sum_binary(Bin, C+Sum);
sum_binary(<<>>, Sum) -> 
    Sum.

make_message(Src,Dst,ID) ->
    make_message(Src,Dst,ID,<<>>).

make_message(Src,Dst,ID,Data) ->
    make_message(Src,Dst,ID,Data,undefined).

make_message(Src,Dst,ID,Data,CheckSum) ->
    #message { packet_len = 3+erlang:iolist_size(Data),
	       source_device=device_encode(Src),
	       destination_device=device_encode(Dst),
	       message_id = ID,
	       message_data = Data,
	       checksum = CheckSum }.



-define(MAP(K,V), K => V).
-define(device_map, #{ ?DEVICE_MAP }).
device_encode(Name) when is_atom(Name) ->  maps:get(Name, ?device_map);
device_encode(Dev) when is_integer(Dev) -> Dev.
-undef(MAP).

-define(MAP(K,V), V => K).
-define(device_rev_map, #{ ?DEVICE_MAP }).
device_decode(Dev) when is_integer(Dev) ->
    case maps:find(Dev, ?device_rev_map) of
	error -> Dev;
	{ok,Name} -> Name
    end.
-undef(MAP).


test() ->
    M1 = make_message(hc, azm, 16#fe, <<>>),
    M1c = set_checksum(M1),
    print_message(M1c),
    16#eb = M1c#message.checksum,
    
    M2 = make_message(azm, hc, 16#fe, <<16#04, 16#03>>),
    M2c = set_checksum(M2),
    print_message(M2c),
    16#e2 = M2c#message.checksum,

    M3 = make_message(hc, alt, 16#fe, <<>>),
    M3c = set_checksum(M3),
    print_message(M3c),
    16#ea = M3c#message.checksum,
    
    M4 = make_message(alt, hc, 16#fe, <<16#04, 16#03>>),
    M4c = set_checksum(M4),
    print_message(M4c),
    16#e1 = M4c#message.checksum,

    ok.

message_list(Message) when is_record(Message,message) ->
    tl(tuple_to_list(Message)).

print_message(Message) ->
    io:format("~s\n", [format_message(Message)]).

format_message(Message) ->
    [_|Data] = tuple_to_list(Message),
    [$<,
     drop_first_blank(
       [ [$\s|tl(integer_to_list(Byte+16#100,16))] ||
	   <<Byte>> <= erlang:iolist_to_binary(Data)]),
     $>].

drop_first_blank([[$\s|As]|Bs]) -> [As|Bs].
