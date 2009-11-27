%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc TEMPLATE.

-module(swarm_locations).
-author('jrbedard <jrbedard@gmail.com>').

-include_lib("stdlib/include/qlc.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("swarm_schema.hrl").

-export([request/1, insert_test_data/0]).


request(Args) ->
	case Args of
        "around/lat=" ++ Lat ->
			get_locations_around(Lat, 32.2, true);
		
		"favorite/" ++ Person ->
			get_favorite_locations(Person);
		
		_ ->
			"<error>Not found</error>"
	end.


%%  L O C A T I O N S

%% Get Locations around a position
get_locations_around(Lat, Lgn, Offline) ->
	Rows = swarm_mnesia:do(qlc:q([X || X <- mnesia:table(location)])),
	LocationsXML = locations_to_xml(Rows).


%% Get Favorite Locations
get_favorite_locations(P_id) ->
	Rows = swarm_mnesia:do(qlc:q([X || X <- mnesia:table(location)])),
	LocationsXML = locations_to_xml(Rows).


locations_to_xml(Rows) ->
	%% Create Locations XML
	{RootEl, _} = xmerl_scan:string("<locations/>"),
	#xmlElement{content = Content} = RootEl,

	%% For each Person Row
	NewContent = export_location(Rows, Content, 0),
	lists:flatten([NewContent]),
	%%io:format("location xml : ~p~n", [NewContent]),

	NewRootEl=RootEl#xmlElement{content=NewContent},
	LocationsXML=xmerl:export_simple([NewRootEl], xmerl_xml),
	lists:flatten(LocationsXML).
	%%io:format("location xml : ~p~n", [Export]),


%% Recursive location Export
export_location([], Data, Type) ->
	Data;
export_location([Row | NewRows], OldData, Type) ->
	case Type of
	0 -> %% location around / favorites
	Data = [{location, [], [
			{id, [], [Row#location.id]},
			{name, [], [Row#location.name]},
			{image_url, [], [Row#location.image_url]}
		   ]}]
	end,
	NewData = OldData ++ Data,
	export_location(NewRows, NewData, Type).



%%  L O C A T I O N

%% Get Location info
get_location_info(E_id) ->
	Fun = fun() -> mnesia:read({location, E_id}) end,
	{atomic, [Row]} = mnesia:transaction(Fun),

	Data = [{location, [], [
			{id, [], [Row#location.id]},
			{name, [], [Row#location.name]}
		   ]}],

	NewRootEl=#xmlElement{content=Data},
	LocationXML=xmerl:export_simple([NewRootEl], xmerl_xml),
	lists:flatten(LocationXML).


%% Create Location
create_location(E_id, E_name) ->
	Row = #location{id=E_id, name=E_name},
	F = fun() -> mnesia:write(Row) end,
	mnesia:transaction(F).


%% Insert fake locations in Mnesia!
	insert_test_data() ->
	io:format("SWARM: Adding Fake Locations~n"),
	create_location("test","test"),
	create_location("test2","test2").
	%%create_event("test3","test3").
	%% p_at_pos, {person, lat, lgn}).


fake_locations() -> 
		"<people count='2'>
		<person id='1' name='test'>
			<image path='test'/>
			<status online='true'>my status</status>
			<position lat='32.3' lgn='12.4' city='San Francisco'/>
		</person>
		<person id='2' name='test2'>
			<image path='test'/>
			<status online='false'/>
			<position lat='32.3' lgn='12.4' city='San Francisco'/>
		</person>
		<person id='3' name='test3'>
			<image path='test'/>
			<status online='false'/>
			<position lat='32.3' lgn='12.4' city='San Francisco'/>
		</person>
		</people>".
