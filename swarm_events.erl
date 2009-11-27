%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc TEMPLATE.

-module(swarm_events).
-author('jrbedard <jrbedard@gmail.com>').

-include_lib("stdlib/include/qlc.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("swarm_schema.hrl").

-export([request/1, insert_test_data/0]).


request(Args) ->
	case Args of
		"around/lat=" ++ Lat ->
			get_events_around(Lat, 32.2, true);
		
		"favorites/" ++ Person ->
			get_favorite_events(Person);
		
		"event/" ++ Event ->
			get_event_info("test");
			
		_ ->
			"<error>Not found</error>"
	end.


%%  E V E N T S

%% Get Events around a position
get_events_around(Lat, Lgn, Offline) ->
	Rows = swarm_mnesia:do(qlc:q([X || X <- mnesia:table(event)])),
	EventsXML = events_to_xml(Rows).


%% Get Favorite Events
get_favorite_events(p_id) ->
	Rows = swarm_mnesia:do(qlc:q([X || X <- mnesia:table(event)])),
	EventsXML = events_to_xml(Rows).


events_to_xml(Rows) ->
	%% Create People XML
	{RootEl, _} = xmerl_scan:string("<events/>"),
	#xmlElement{content = Content} = RootEl,

	%% For each Person Row
	NewContent = export_event(Rows, Content, 0),
	lists:flatten([NewContent]),
	%%io:format("people xml : ~p~n", [NewContent]),

	NewRootEl=RootEl#xmlElement{content=NewContent},
	EventsXML=xmerl:export_simple([NewRootEl], xmerl_xml),
	lists:flatten(EventsXML).
	%%io:format("people xml : ~p~n", [Export]),


%% Recursive person Export
export_event([], Data, Type) ->
	Data;
export_event([Row | NewRows], OldData, Type) ->
	case Type of
	0 -> %% event around / favorites
	Data = [{event, [], [
			{id, [], [Row#event.id]},
			{name, [], [Row#event.name]},
			{image_url, [], [Row#event.image_url]}
		   ]}]
	end,
	NewData = OldData ++ Data,
	export_event(NewRows, NewData, Type).



%%  E V E N T

%% Get Event info
get_event_info(E_id) ->
	Fun = fun() -> mnesia:read({event, E_id}) end,
	{atomic, [Row]} = mnesia:transaction(Fun),

	Data = [{event, [], [
			{id, [], [Row#event.id]},
			{name, [], [Row#event.name]}
		   ]}],

	NewRootEl=#xmlElement{content=Data},
	EventXML=xmerl:export_simple([NewRootEl], xmerl_xml),
	lists:flatten(EventXML).


%% Create Event
create_event(E_id, E_name) ->
	Row = #event{id=E_id, name=E_name},
	F = fun() -> mnesia:write(Row) end,
	mnesia:transaction(F).



%% Insert fake users in Mnesia!
insert_test_data() ->
	io:format("SWARM: Adding Fake Events~n"),
	create_event("test","test"),
	create_event("test2","test2").
	%%create_event("test3","test3").
	%% p_at_pos, {person, lat, lgn}).



fake_events() -> 
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