%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%     Calculate planetary positions
%%%     Reference: http://stjarnhimlen.se/comp/ppcomp.html
%%% @end
%%% Created :  1 Nov 2014 by Tony Rogvall <tony@rogvall.se>

-module(eplanet).

-export([j2000_days/0, j2000_days/1,
	 j2000_days_1/1,
	 j2000_to_gregorian_seconds/1,
	 j2000_to_datetime/1,
	 gmst/1,
	 mean_sidereal_time/2,
	 local_sidereal_time/2,
	 longitude_to_deg/1,
	 latitude_to_deg/1,
	 hours_to_decimal/1,
	 deg_to_decimal/1,

	 ra_dec/2,
	 right_ascension_and_declination/2,
	 local_hour_angle/3, 
	 azimuthal_coordinates/4,
	 obliquity_of_the_ecliptic/1,

	 orbit/2,
	 orbital_period/2,
	 mean_distance/2,
	 eccentricity/2,
	 inclination/2,
	 argument_of_perihelion/2,
	 mean_anomaly/2,
	 eccentric_anomaly/2,
	 longitude_of_ascending_node/2,

	 distance/2,
	 position_orb/2,
	 position_ecl/2,
	 position_eq/2,
	 position_geo/2,

	 mass/1,
	 diameter/1,
	 radius/1,
	 density/1,
	 gravity/1
	 ]).

-export([location/0,
	 get_time_zone/0,
	 search_time_zone/2,
	 location_from_zone_tab/1,
	 day/0,
	 azimuthal_coordinates/2
	]).

-define(PI, 3.14159265358979323846).
-define(RADDEG, (180.0/?PI)).
-define(DEGRAD, (?PI/180.0)).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).     %% 60*60*24
-define(DAYS_PER_CENTURIES, 36525.0).  %% 365*100 + 25
-define(J2000_SECONDS, 63113817600). %% gregorian seconds 1999-12-31 00:00:00

-type deg()   :: float().
-type rad()   :: float().
-type au()    :: number().         %% astronomical units

-type years() :: number().

-type hour_t()   :: 0..23.
-type minute_t() :: 0..59.
-type second_t() :: 0..59.
-type time_t()   :: {hour_t(),minute_t(),second_t()}.
-type day_t()    :: 1..31.
-type month_t()  :: 1..12.
-type year_t()  :: 0000..9999.
-type date_t()  :: {year_t(), month_t(), day_t()}.
-type date_time_t() :: {date_t(), time_t()}.

-type j2000_t() :: number().       %% j2000 days
-type jday_t() :: j2000_t()|date_t()|date_time_t().

-type planet() :: mercury | venus | earth | mars | jupiter | saturn |
		  uranus | netptune | pluto.
-type body_t() :: planet() | moon | sun.

%% orbital paramerts
-record(orbital,
	{
	  name :: body_t(),
	  a0 :: float(),    %% semi-major axis (au)
	  e0 :: float(),    %% eccentrcity
	  i0 :: float(),    %% inclination i (deg)
	  l0 :: float(),    %% mean longitude (deg)
	  w0 :: float(),    %% longitude of perihelion (deg)
	  o0 :: float(),    %% longitude of the ascending node (N) (deg)

	  ad :: float(),    %% semi-major axis (au/day)
	  ed :: float(),    %% eccentrcity (rad/day)
	  id :: float(),    %% inclination (deg/day)
	  ld :: float(),    %% mean longitude (deg/day)
	  wd :: float(),    %% longitude of perihelion (deg/day)
	  od :: float(),    %% longitude of the ascending node (deg/day)

	  %% coefficents for extra terms
	  %% if b =:= 0 then code asssume that all are c,s,f also are 0!!!

	  b=0 :: number(),
	  c=0 :: number(),
	  s=0 :: number(),
	  f=0  :: number()
	}).

-define(ORBIT(Name,A0,E0,I0,L0,W0,O0, Ad,Ed,Id,Ld,Wd,Od),
	#orbital { name=(Name),
		   a0=(A0),e0=(E0),i0=(I0),l0=(L0),w0=(W0),o0=(O0),
		   ad=(Ad),ed=(Ed),id=(Id),ld=(Ld),wd=(Wd),od=(Od)
		 }).


-define(ORBIT_X(Name,A0,E0,I0,L0,W0,O0, Ad,Ed,Id,Ld,Wd,Od,
		B,C,S,F),
	#orbital { name=(Name),
		   a0=(A0),e0=(E0),i0=(I0),l0=(L0),w0=(W0),o0=(O0),
		   ad=(Ad),ed=(Ed),id=(Id),ld=(Ld),wd=(Wd),od=(Od)
		   b=(B),c=(C),s=(S),f=(F)
		 }).

%% deltas are given in deg/centuary so we rescale into deg/day
-define(ORBIT_C(Name,A0,E0,I0,L0,W0,O0, Ad,Ed,Id,Ld,Wd,Od),
	#orbital { name=(Name),
		   a0=(A0),e0=(E0),i0=(I0),l0=(L0),w0=(W0),o0=(O0),
		   ad=(Ad)/?DAYS_PER_CENTURIES,
		   ed=(Ed)/?DAYS_PER_CENTURIES,
		   id=(Id)/?DAYS_PER_CENTURIES,
		   ld=(Ld)/?DAYS_PER_CENTURIES,
		   wd=(Wd)/?DAYS_PER_CENTURIES,
		   od=(Od)/?DAYS_PER_CENTURIES
		 }).

-define(ORBIT_CX(Name,A0,E0,I0,L0,W0,O0, Ad,Ed,Id,Ld,Wd,Od,
		B,C,S,F),
	#orbital { name=(Name),
		   a0=(A0),e0=(E0),i0=(I0),l0=(L0),w0=(W0),o0=(O0),
		   ad=(Ad)/?DAYS_PER_CENTURIES,
		   ed=(Ed)/?DAYS_PER_CENTURIES,
		   id=(Id)/?DAYS_PER_CENTURIES,
		   ld=(Ld)/?DAYS_PER_CENTURIES,
		   wd=(Wd)/?DAYS_PER_CENTURIES,
		   od=(Od)/?DAYS_PER_CENTURIES,
		   b=(B)/(?DAYS_PER_CENTURIES*?DAYS_PER_CENTURIES),
		   c=(C), 
		   s=(S), 
		   f=(F)/?DAYS_PER_CENTURIES
		 }).

location() ->
    Offs = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case get_time_zone() of
	{ok, TimeZone} ->
	    case location_from_zone_tab(TimeZone) of
		{ok,{_CountryCodes,{Latitude,Longitude},_TZ}} ->
		    {Latitude, Longitude, Offs / ?SECONDS_PER_HOUR}
	    end
    end.

day() -> %% now
    {date(), time()}.

%% Long = longitude_to_deg({1,55,0,1}).
%% -1.9166666666666665

%% Lat = latitude_to_deg({52,30,0,0}).
%% 52.5

%% caluctate number of days before Now

j2000_days() ->
    j2000_days(calendar:now_to_datetime(os:timestamp())).

j2000_days(Days) when is_number(Days) -> %% assume already converted
    Days;
j2000_days({Days,Time}) when is_number(Days), is_number(Time) ->
    Days + Time;
j2000_days(Date={YYYY,MM,DD}) %% from date
  when is_integer(YYYY), YYYY >= 0, YYYY =< 9999,
       is_integer(MM), MM >= 1, MM =< 12,
       is_integer(DD), DD >= 1, DD =< 31 ->
    S0 = calendar:datetime_to_gregorian_seconds({Date,{0,0,0}}),
    (S0 - ?J2000_SECONDS) / ?SECONDS_PER_DAY;
j2000_days(DateTime={{YYYY,MM,DD},{H,M,S}}) 
  when is_integer(YYYY), YYYY >= 0, YYYY =< 9999,
       is_integer(MM), MM >= 1, MM =< 12,
       is_integer(DD), DD >= 1, DD =< 31,
       is_integer(H), H >= 0, H =< 23,
       is_integer(M), M >= 0, H =< 59,
       is_integer(S), S >= 0, S =< 59 ->
    S0 = calendar:datetime_to_gregorian_seconds(DateTime),
    (S0 - ?J2000_SECONDS) / ?SECONDS_PER_DAY.

j2000_to_gregorian_seconds(Days) ->
    trunc(Days*?SECONDS_PER_DAY + ?J2000_SECONDS).

j2000_to_datetime(Days) ->
    S = j2000_to_gregorian_seconds(Days),
    calendar:gregorian_seconds_to_datetime(S).

%% faster?
j2000_days_1({YYYY,MM,DD}) 
  when is_integer(YYYY), YYYY >= 0, YYYY =< 9999,
       is_integer(MM), MM >= 1, MM =< 12,
       is_integer(DD), DD >= 1, DD =< 31 ->
    j2000_date_days(YYYY,MM,DD);
j2000_days_1({{YYYY,MM,DD},{H,M,S}}) 
  when is_integer(YYYY), YYYY >= 0, YYYY =< 9999,
       is_integer(MM), MM >= 1, MM =< 12,
       is_integer(DD), DD >= 1, DD =< 31,
       is_integer(H), H >= 0, H =< 23,
       is_integer(M), M >= 0, H =< 59,
       is_integer(S), S >= 0, S =< 59 ->
    j2000_date_days(YYYY,MM,DD) + time_to_days(H,M,S).


j2000_date_days(Y,M,D) ->
    367*Y - (7*(Y + ((M+9) div 12))) div 4 + (275*M) div 9 + D - 730530.

time_to_days(H,M,S) ->
    ((H*60+M)*60 + S) / ?SECONDS_PER_DAY.
    
gmst(Day) ->
    D = j2000_days(Day),
    18.697374558 + 24.06570982441908*D.

mean_sidereal_time(Day, Longitude) ->
    JD = j2000_days(Day),
    JT = JD/?DAYS_PER_CENTURIES,
    Mst = 280.46061837 + 360.98564736629*JD + 
	0.000387933*JT*JT - JT*JT*JT/38710000 + longitude_to_deg(Longitude),
    mod360(Mst).
    
%%
%% Local Sidereal Time
%%
%% D          the days from J2000, including the fraction of a day
%% Longitude  longitude in decimal degrees, East positive.
%%
-spec local_sidereal_time(Day::jday_t(), Longitude::deg()) -> deg().
				 
local_sidereal_time(Day, Longitude) ->
    D = j2000_days(Day),
    UT = 24*(D - trunc(D)),
    mod360(100.46 + 0.985647*trunc(D) + longitude_to_deg(Longitude) + 15*UT).

%%
%% Distance to planet from sun system center
%% and also the angle.
-spec distance(Body::body_t(), D::jday_t()) ->
		      {R::float(), V::float()}.
distance(Body, D) ->
    {X,Y,_Z} = position_orb(Body, D),
    V = math:atan2(Y, X),
    R = math:sqrt(X*X + Y*Y),
    {R, V}.

%%
%% Heliocentric coordinates in the bodys orbital plane, r', with
%% the x'-axos aligned from the focus to the perihelion
%%
%% r' : x' = a(cos E - e) y' = a*sqrt(1 - e^2) sin(E) z' = 0
%%

-spec position_orb(Body::body_t(), Day::jday_t()) ->
			  {X::float(), Y::float(), Z::float()}.
position_orb(Body, Day) ->
    D = j2000_days(Day),
    R = orbit(Body,D),
    E = eccentric_anomaly_(R, D),
    _e = eccentricity_(R, D),
    A = mean_distance_(R, D),
    X = A * (cosd(E) - _e),
    Y = A * (math:sqrt(1.0 - _e*_e) * sind(E)),
    {X, Y, 0.0}.

%%
%% The coordinates recl, in the J2000 ecliptic plane, with the x-asis
%% towards the equinox
%%
%% R = Mr = Rz(-O)Rx(-I)Rz(-w)r'
%%
-spec position_ecl(Body::body_t(), Day::jday_t()) ->
			  {X::float(),Y::float(),Z::float()}.
position_ecl(Body, Day) ->
    D = j2000_days(Day),
    {X0,Y0,_Z0} = position_orb(Body, D),
    R = orbit(Body,D),
    O = longitude_of_ascending_node_(R, D),  %% omega
    I = inclination_(R, D),
    W = argument_of_perihelion_(R, D),
    CW = cosd(W), CO = cosd(O), CI = cosd(I),
    SW = sind(W), SO = sind(O), SI = sind(I),
    SOCI = SO*CI,
    COCI = CO*CI,
    X = (CW*CO - SW*SOCI)*X0 + (-SW*CO - CW*SOCI)*Y0,
    Y = (CW*SO + SW*COCI)*X0 + (-SW*SO + CW*COCI)*Y0,
    Z = (SW*SI)*X0 +            ( CW*SI)*Y0,
    {X, Y, Z}.


%%
%% Calculate geocentric position of the body
%%
-spec position_geo(Body::body_t(), Day::jday_t()) ->
    {X::float(),Y::float(),Z::float()}.

position_geo(Body, Day) ->
    D = j2000_days(Day),
    {Xh,Yh,Zh} = position_ecl(Body, D),
    {Xs,Ys,Zs} = position_ecl(sun, D),
    {Xh+Xs,Yh+Ys,Zh+Zs}.

%%
%% Equatorial coordinates in the ICRF/J2000 frame 
%%
-spec position_eq(Body::body_t(), Day::jday_t()) ->
			 {X::float(),Y::float(),Z::float()}.

position_eq(Body, Day) ->
    D = j2000_days(Day),
    {Xg,Yg,Zg} = position_geo(Body, D),
    E = obliquity_of_the_ecliptic(D),
    SE = sind(E),
    CE = cosd(E),
    X = Xg,
    Y = Yg*CE - Zg*SE,
    Z = Yg*SE + Zg*CE,
    {X, Y, Z}.

-spec right_ascension_and_declination(Body::body_t(), Day::jday_t()) ->
					     {RA::deg(), Dec::deg()}.

ra_dec(Body, Day) -> %% alias
    right_ascension_and_declination(Body, Day).
    
right_ascension_and_declination(Body, Day) ->
    D = j2000_days(Day),
    {Xe,Ye,Ze} = position_eq(Body, D),
    RA = math:atan2( Ye, Xe ),
    Decl = math:atan2( Ze, math:sqrt(Xe*Xe+Ye*Ye)),
    {RA*?RADDEG, Decl*?RADDEG}.

-spec local_hour_angle(Body::body_t(),Day::jday_t(),Longitude::deg()) -> deg().

local_hour_angle(Body, Day, Longitude) ->
    {RA, _Decl} = right_ascension_and_declination(Body, Day),
    Long = longitude_to_deg(Longitude),
    LST = local_sidereal_time(Day, Long),
    mod360(LST - RA) - 180.


-spec azimuthal_coordinates(Body::body_t(), Day::jday_t(),
			    Longitude::deg(), Latitude::deg()) ->
				   {Az::deg(), Alt::deg()}.
				   
azimuthal_coordinates(Body, Day, Longitude, Latitude) ->
    {RA, Decl} = right_ascension_and_declination(Body, Day),
    Long = longitude_to_deg(Longitude),
    LST = local_sidereal_time(Day, Long),
    HA = mod360(LST - RA) - 180,
    X = cosd(HA)*cosd(Decl),
    Y = sind(HA)*cosd(Decl),
    Z = sind(Decl),
    Lat = latitude_to_deg(Latitude),
    Xhor = X * sind(Lat) - Z*cosd(Lat),
    Yhor = Y,
    Zhor = X * cosd(Lat) + Z*sind(Lat),
    
    Az  = atan2d(Yhor, Xhor) + 180.0,
    Alt = atan2d(Zhor, math:sqrt(Xhor*Xhor + Yhor*Yhor)),
    {Az, Alt}.

azimuthal_coordinates(Body, Day) ->
    {Longitude, Latitude, _} = location(),
    azimuthal_coordinates(Body, Day, Longitude, Latitude).

%%
%% Earth's axis of rotation
%%
-spec obliquity_of_the_ecliptic(Day::jday_t()) -> deg.

obliquity_of_the_ecliptic(Day) ->
    D = j2000_days(Day),
    23.4393 - 3.563E-7*D.

%%
%% Plantery body information
%%

%%
%% Mean distance in astronomical units (au).
%%
-spec mean_distance(Body::body_t(), Day::jday_t()) -> au().
mean_distance(Body, Day) ->
    D = j2000_days(Day),
    mean_distance_(orbit(Body,D), D).

-spec mean_distance_(R::#orbital{}, D::j2000_t()) -> au().
mean_distance_(R, D) ->
    R#orbital.a0 + D*R#orbital.ad.

%% eccentricity e
-spec eccentricity(Body::body_t(), Day::jday_t()) -> au().
eccentricity(Body, Day) ->
    D = j2000_days(Day),
    eccentricity_(orbit(Body,D), D).

-spec eccentricity_(R::#orbital{}, D::j2000_t()) -> rad().
eccentricity_(R, D) ->
    R#orbital.e0 + D*R#orbital.ed.

%% Inclination i
-spec inclination(Body::body_t(), Day::jday_t()) -> deg().
inclination(Body, Day) ->
    D = j2000_days(Day),
    inclination_(orbit(Body,D), D).

-spec inclination_(R::#orbital{}, D::j2000_t()) -> deg().
inclination_(R, D) ->
    mod360(R#orbital.i0 + D*R#orbital.id).

%% Argument of perihelion (w)
-spec argument_of_perihelion(Body::body_t(), Day::jday_t()) -> deg().
argument_of_perihelion(Body, Day) ->
    D = j2000_days(Day),
    argument_of_perihelion_(orbit(Body,D), D).

-spec argument_of_perihelion_(R::#orbital{}, D::j2000_t()) -> deg().
argument_of_perihelion_(R, D) ->
    W = R#orbital.w0 + D*R#orbital.wd,
    O = R#orbital.o0 + D*R#orbital.od,
    mod360(W - O).

%% Mean anomaly M  = L - W

-spec mean_anomaly(Body::body_t(), Day::jday_t()) -> deg().
mean_anomaly(Body, Day) ->
    D = j2000_days(Day),
    mean_anomaly_(orbit(Body,D), D).

-spec mean_anomaly_(R::#orbital{}, D::j2000_t()) -> deg().
mean_anomaly_(R, D) ->
    L = R#orbital.l0 + D*R#orbital.ld,
    W = R#orbital.w0 + D*R#orbital.wd,
    X =  if R#orbital.b =:= 0 -> 0.0;
	    true ->
		 R#orbital.b*D*D + 
		     R#orbital.c*cosd(R#orbital.f*D) + 
		     R#orbital.s*sind(R#orbital.f*D)
	 end,
    mod360(L - W + X) - 180.
%%
%% calculate E in the equation M = E - e sin E
%%
-spec eccentric_anomaly(Body::body_t(), Day::jday_t()) -> deg().
eccentric_anomaly(Body, Day) ->
    D = j2000_days(Day),
    eccentric_anomaly_(orbit(Body,D), D).

-spec eccentric_anomaly_(R::#orbital{}, D::j2000_t()) -> deg().
eccentric_anomaly_(R, D) ->
    M = mean_anomaly_(R, D),    %% mean anomaly (degrees)
    E = eccentricity_(R, D),    %% eccentricity in radians
    Edeg  = E*?RADDEG,           %% eccentricity deg
    E0 = M + Edeg*sind(M),
    kepler_(E0, M, E, Edeg, 1.0E-6).

kepler_(En, M, E, Edeg, Tol) ->
    Md = M - (En - Edeg*sind(En)),
    Ed = Md/(1 - E*cosd(En)),
    En1 = En + Ed,
    if abs(Ed) < Tol ->
	    En1;
       true ->
	    kepler_(En1,M,E,Edeg,Tol)
    end.

%% Longitude of the ascending node
%% N (in stjarnhimelen.se)  or O (jpl.nasa)
%% 
-spec longitude_of_ascending_node(Body::body_t(),Day::jday_t()) -> deg().
longitude_of_ascending_node(Body, Day) ->
    D = j2000_days(Day),
    longitude_of_ascending_node_(orbit(Body,D), D).

-spec longitude_of_ascending_node_(R::#orbital{}, D::j2000_t()) -> deg().
longitude_of_ascending_node_(R,D) ->
    O = R#orbital.o0 + D*R#orbital.od,
    mod360(O).

%% Calculate number of years for one lap around the sun / earth ...
-spec orbital_period(Body::body_t(), Day::jday_t()) -> years().
orbital_period(Body, Day) ->
    D = j2000_days(Day),
    A = mean_distance(Body,D),
    math:pow(A, 1.5).

%% mass relative to earth mass
mass(mercury) -> 0.0553;
mass(venus) -> 0.815;
mass(earth) -> 1.00;  %% 5,9726 × 1024 kg
mass(moon) -> 0.0123;
mass(mars) -> 0.107;
mass(jupiter) -> 317.8;
mass(saturn) -> 95.2;
mass(uranus) -> 14.5;
mass(neuptune) -> 17.1;
mass(pluto) -> 0.0025.

density(mercury) -> 0.984;
density(venus) -> 0.951;
density(earth) -> 1.00;
density(moon) -> 0.605;
density(mars) -> 0.713;
density(jupiter) -> 0.240;
density(saturn) -> 0.125;
density(uranus) -> 0.230;
density(neptune) -> 0.297;
density(pluto) -> 0.380.

gravity(mercury) -> 0.378;
gravity(venus) -> 0.907;
gravity(earth) -> 1.00;
gravity(moon) -> 0.166;
gravity(mars) -> 0.377;
gravity(jupiter) -> 2.36;
gravity(saturn) -> 0.916;
gravity(uranus) -> 0.889;
gravity(neptune) -> 1.12;
gravity(pluto) -> 0.071.

diameter(mercury)  -> 0.382;
diameter(venus)    -> 0.949;
diameter(earth)    -> 1.00;  %% 6 371,0 Km
diameter(moon) -> 0.2724;
diameter(mars)     -> 0.532;
diameter(jupiter)  -> 11.21;
diameter(saturn)   -> 9.45;
diameter(uranus)   -> 4.01;
diameter(neuptune) -> 3.88;
diameter(pluto)    -> 0.186. %% 2300 Km

radius(Body) ->
    diameter(Body)/2.

%% select orbit table
-spec orbit(Body::body_t(), Day::jday_t()) -> #orbital {}.

orbit(Body,Day) ->
    D = j2000_days(Day),
    if D =< 18228, D >= -73047 -> orbit_1(Body);    %% 1800 AD - 2040 AD
       D =< 365608, D >= -730484 -> orbit_2(Body)   %% 3000 BC - 3000 AD
    end.

%%
%% orbital elements for 1800 AD - 2050 AD. J2000 (-73047 ... 18628 )
%%
orbit_1(sun) ->
    N0 = 0.0, Nd = 0.0,
    I0 = 0.0, Id = 0.0,
    W0 = 282.9404, Wd = 4.70935E-5,
    A0 = 1.000000, Ad = 0.0,
    E0 = 0.016709, Ed = -1.151E-9,
    M0 = 356.0470, Md = 0.9856002585,
    L0 = M0 + N0 + W0, Ld = Md+Nd+Wd,
    ?ORBIT(sun,
	   A0, E0, I0, L0, W0, N0,
	   Ad, Ed, Id, Ld, Wd, Nd);

orbit_1(moon) ->
    N0 = 125.1228, Nd = - 0.0529538083,
    I0 = 5.1454, Id = 0.0,
    W0 = 318.0634, Wd = 0.1643573223,
    A0 = 60.2666, Ad = 0.0, %%  (Earth radii)
    E0 = 0.054900, Ed = 0.0,
    M0 = 115.3654, Md = 13.0649929509,
    L0 = M0 + N0 + W0, Ld = Md+Nd+Wd,
    ?ORBIT(moon, 
	   A0, E0, I0, L0, W0, N0,
	   Ad, Ed, Id, Ld, Wd, Nd);

orbit_1(mercury) ->
    ?ORBIT_C(mercury,
	     0.38709927, 0.20563593, 7.00497902,    252.25032350, 77.45779628, 48.33076593,
	     0.00000037, 0.00001906,-0.00594749, 149472.67411175,  0.16047689, -0.12534081);
orbit_1(venus) ->
    ?ORBIT_C(venus,
	   0.72333566, 0.00677672, 3.39467605,   181.97909950, 131.60246718, 76.67984255,
	   0.00000390,-0.00004107,-0.00078890, 58517.81538729,   0.00268329, -0.27769418);
orbit_1(earth) -> %% EM Bary
    ?ORBIT_C(earth,
	   1.00000261, 0.01671123,-0.00001531,   100.46457166, 102.93768193, 0.0,
	   0.00000562,-0.00004392,-0.01294668, 35999.37244981,   0.32327364, 0.0);
orbit_1(mars) -> 
    ?ORBIT_C(mars,
	   1.52371034,      0.09339410,      1.84969142,       -4.55343205,    -23.94362959,     49.55953891,
	   0.00001847,      0.00007882,     -0.00813131,    19140.30268499,      0.44441088,     -0.29257343);
orbit_1(jupiter) -> 
    ?ORBIT_C(jupiter,
	    5.20288700,      0.04838624,      1.30439695,       34.39644051,     14.72847983,    100.47390909,
	    -0.00011607,     -0.00013253,     -0.00183714,     3034.74612775,      0.21252668,      0.20469106);
orbit_1(saturn) -> 
    ?ORBIT_C(saturn,
	   9.53667594,      0.05386179,      2.48599187,       49.95424423,     92.59887831,    113.66242448,
	   -0.00125060,     -0.00050991,      0.00193609,     1222.49362201,     -0.41897216,     -0.28867794);
orbit_1(uranus) ->
    ?ORBIT_C(uranus,
	   19.18916464,      0.04725744,      0.77263783,      313.23810451,    170.95427630,     74.01692503,
	   -0.00196176,     -0.00004397,     -0.00242939,      428.48202785,      0.40805281,      0.04240589);
orbit_1(neptune) ->
    ?ORBIT_C(neptune,
	   30.06992276,      0.00859048,      1.77004347,      -55.12002969,     44.96476227,    131.78422574,
	   0.00026291,      0.00005105,      0.00035372,      218.45945325,     -0.32241464,     -0.00508664);
orbit_1(pluto) ->    
    ?ORBIT_C(pluto,
	   39.48211675,      0.24882730,     17.14001206,      238.92903833,    224.06891629,    110.30393684,
	   -0.00031596,      0.00005170,      0.00004818,      145.20780515,     -0.04062942,     -0.01183482).


%%
%% orbital elements for 3000 BC - 3000 AD. J2000 (-73047 ... 18628 )
%% (fixme how to handle 3000 BC ?)
%% for now assume 0 - 3000 (-730484 ...365608)
%%
orbit_2(sun) -> orbit_1(sun);
orbit_2(moon) -> orbit_1(moon);

orbit_2(mercury) -> 
    ?ORBIT_C(mercury,
	   0.38709843,      0.20563661,      7.00559432,      252.25166724,     77.45771895,     48.33961819,
	   0.00000000,      0.00002123,     -0.00590158,   149472.67486623,      0.15940013,     -0.12214182);
orbit_2(venus) ->
    ?ORBIT_C(venus,
	   0.72332102,      0.00676399,      3.39777545,      181.97970850,    131.76755713,     76.67261496,
	   -0.00000026,     -0.00005107,      0.00043494,    58517.81560260,      0.05679648,     -0.27274174);
orbit_2(earth) ->
    ?ORBIT_C(earth,
	   1.00000018,      0.01673163,     -0.00054346,      100.46691572,    102.93005885,     -5.11260389,
	   -0.00000003,     -0.00003661,     -0.01337178,    35999.37306329,      0.31795260,     -0.24123856);
orbit_2(mars) ->
    ?ORBIT_C(mars,
	   1.52371243,      0.09336511,      1.85181869,       -4.56813164,    -23.91744784,     49.71320984,
	   0.00000097,      0.00009149,     -0.00724757,    19140.29934243,      0.45223625,     -0.26852431);
orbit_2(jupiter) ->
    ?ORBIT_CX(jupiter,
	    5.20248019,      0.04853590,      1.29861416,       34.33479152,     14.27495244,    100.29282654,
	    -0.00002864,      0.00018026,     -0.00322699,     3034.90371757,      0.18199196,      0.13024619,
	    -0.00012452,   0.06064060,   -0.35635438,   38.35125000);
orbit_2(saturn) ->
    ?ORBIT_CX(saturn,
	    9.54149883,      0.05550825,      2.49424102,       50.07571329,     92.86136063,    113.63998702,
	    -0.00003065,     -0.00032044,      0.00451969,     1222.11494724,      0.54179478,     -0.25015002,
	    0.00025899,   -0.13434469,    0.87320147,   38.35125000);
orbit_2(uranus) ->
    ?ORBIT_CX(uranus,
	    19.18797948,      0.04685740,      0.77298127,      314.20276625,    172.43404441,     73.96250215,
	    -0.00020455,     -0.00001550,     -0.00180155,      428.49512595,      0.09266985,      0.05739699,
	    0.00058331,  -0.97731848,    0.17689245,    7.67025000);
orbit_2(neptune) ->
    ?ORBIT_CX(neptune,
	    30.06952752,      0.00895439,      1.77005520,      304.22289287,     46.68158724,    131.78635853,
	    0.00006447,      0.00000818,      0.00022400,      218.46515314,      0.01009938,     -0.00606302,
	    -0.00041348,    0.68346318,   -0.10162547,    7.67025000);
orbit_2(pluto) ->
    ?ORBIT_CX(pluto,
	    39.48686035,      0.24885238,     17.14104260,      238.96535011,    224.09702598,    110.30167986,
	    0.00449751,      0.00006016,      0.00000501,      145.18042903,     -0.00968827,     -0.00809981,
	    -0.01262724,    0,            0,           0).

sind(Deg) -> math:sin(Deg*?DEGRAD).
cosd(Deg)  -> math:cos(Deg*?DEGRAD).
%%asind(Sin) -> ?RADDEG*math:asin(Sin).
%%acosd(Cos) -> ?RADDEG*math:acos(Cos).
atan2d(Y,X) -> ?RADDEG*math:atan2(Y,X).
-ifdef(__not_used__).
atand(X) ->    ?RADDEG*math:atan(X).
-endif.

%% fixme: allow various formats

%% 0 = east, 1 = west, convert longitude to degree (0..
longitude_to_deg({Deg,Min,Sec,W}) when 
      W =:= 1; W =:= $W; W =:= $- ->  %% West = 1
    -deg_to_decimal(Deg,Min,Sec);
longitude_to_deg({Deg,Min,Sec,W}) 
  when W =:= 0; W =:= $E; W =:= $+ ->  %% East = 0
    deg_to_decimal(Deg,Min,Sec);
longitude_to_deg(Deg) when is_number(Deg) ->
    Deg.

latitude_to_deg({Deg,Min,Sec,S}) 
  when S =:= 1; S =:= $S; S =:= $- ->  %% South = 1
    -deg_to_decimal(Deg,Min,Sec);
latitude_to_deg({Deg,Min,Sec,S}) 
  when S =:= 0; S =:= $N; S =:= $+ ->  %% North = 0
    deg_to_decimal(Deg,Min,Sec);
latitude_to_deg(Deg) when is_number(Deg) ->
    Deg.

hours_to_decimal({H,M,S}) ->
    H + M/60 + S/(60*60);
hours_to_decimal(H) when is_number(H) ->
    H.    

deg_to_decimal({Deg,Min,Sec}) ->
    deg_to_decimal(Deg,Min,Sec);
deg_to_decimal(Degree) when is_number(Degree) ->
    Degree.

deg_to_decimal(Deg,Min,Sec) ->
    Deg + Min/60 + Sec/(60*60).

mod360(X) ->
    R = X - trunc(X/360)*360,
    if R < 0 -> R + 360.0;
       true -> R
    end.

-ifdef(__not_used__).
cbrt(X) when X > 0 ->
    math:exp(math:log(X) / 3.0);
cbrt(X) when X < 0 ->
    -cbrt(-X);
cbrt(_X) ->
    0.0.
-endif.

get_time_zone() ->
    case file:read_file("/etc/timezone") of
	{error, enoent} ->
	    case file:read_link("/etc/localtime") of
		{ok,"/usr/share/zoneinfo/"++Zone} ->
		    {ok,Zone};
		{error,enoent} ->
		    {error,enoent};
		{error,einval} ->
		    case file:read_file("/etc/localtime") of
			{ok,ZoneInfo} ->
			    MD5 = erlang:md5(ZoneInfo),
			    search_time_zone("/usr/share/zoneinfo", MD5);
			Error ->
			    Error
		    end
	    end;
	{ok,BinZone} -> {ok, binary_to_list(BinZone)}
    end.

search_time_zone(Dir, MD5) ->
    try
	filelib:fold_files(Dir, ".*", true, 
			   fun(File,Acc) ->
				   case file:read_file(File) of
				       {ok,ZoneInfo} ->
					   case erlang:md5(ZoneInfo) of
					       MD5 -> throw(File);
					       _ -> Acc
					   end;
				       _ -> Acc
				   end
			   end, {error,enoent}) of
	Error -> Error
    catch
	throw:"/usr/share/zoneinfo/"++File ->
	    {ok,File}
    end.


location_from_zone_tab(TimeZone) ->
    case file:open(filename:join(code:priv_dir(?MODULE),"zone1970.tab"),
		   [raw,read,binary]) of
	{ok,Fd} ->
	    try location_from_zone_fd(Fd,TimeZone) of
		Zone -> Zone
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

location_from_zone_fd(Fd, TimeZone) ->
    case file:read_line(Fd) of
	{ok, <<$#,_/binary>>} ->
	    location_from_zone_fd(Fd, TimeZone);
	{ok, Data} ->
	    case binary:split(Data, <<"\t">>, [global]) of
		[Codes,Coord,TZ | _Comments] ->
		    case match_tz(TimeZone, trim(binary_to_list(TZ))) of
			true ->
			    {ok, {binary:split(Codes, <<"\t">>, [global]),
				  coord_to_lat_long(Coord),TZ}};
			false ->
			    location_from_zone_fd(Fd, TimeZone)
		    end;
		_ ->
		    location_from_zone_fd(Fd, TimeZone)
	    end;
	eof ->
	    {error,not_found}
    end.

coord_to_lat_long(<<S1,D11,D12,M11,M12,
		    S2,D21,D22,D23,M21,M22>>) when 
      (S1 =:= $+ orelse S1 =:= $-),
      (S2 =:= $+ orelse S2 =:= $-) ->
    Lat = latitude_to_deg({list_to_integer([D11,D12]),
			   list_to_integer([M11,M12]),0,S1}),
    Long = longitude_to_deg({list_to_integer([D21,D22,D23]),
			     list_to_integer([M21,M22]),0,S2}),
    {Lat,Long};
coord_to_lat_long(<<S1,D11,D12,M11,M12,S11,S12,
		    S2,D21,D22,D23,M21,M22,S21,S22>>) ->
    Lat = latitude_to_deg({list_to_integer([D11,D12]),
			   list_to_integer([M11,M12]),
			   list_to_integer([S11,S12]),S1}),
    Long = longitude_to_deg({list_to_integer([D21,D22,D23]),
			     list_to_integer([M21,M22]),
			     list_to_integer([S21,S22]),S2}),
    {Lat,Long}.

trim(Cs) ->
    lists:reverse(trim_(lists:reverse(trim_(Cs)))).

trim_([$\s|Cs]) -> trim_(Cs);
trim_([$\t|Cs]) -> trim_(Cs);
trim_([$\n|Cs]) -> trim_(Cs);
trim_([$\r|Cs]) -> trim_(Cs);
trim_(Cs) -> Cs.

match_tz(TZ, TZ) ->
    true;
match_tz(TZ1, TZ2) ->
    case {string:to_lower(TZ1),string:to_lower(TZ2)} of
	{TZ,TZ} -> true;
	{TZ1L,TZ2L} ->
	    case string:tokens(TZ2L, "/") of
		[_, TZ1L] -> true;
		_ -> false
	    end
    end.
