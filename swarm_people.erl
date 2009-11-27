%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc Swarm people processing module.

-module(swarm_people).
-author('jrbedard <jrbedard@gmail.com>').

-include_lib("stdlib/include/qlc.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("swarm_schema.hrl").

-export([request/1, insert_test_data/0]).


request(Args) ->
	case Args of
        "around/lat=" ++ Lat ->
			get_people_around(Lat, 32.2, true);
		
		"favorites/" ++ Person ->
			get_favorite_people(Person);
		
		"person/" ++ Person ->
			get_person_info("test");
		
		"create/" ++ Person ->
			get_person_info(Person);
		
		"flag/" ++ Person ->
			get_person_info(Person);
		
		"edit_obf6/" ++ Person ->
			get_person_info(Person);
		
		"remove_obf6/" ++ Person ->
			get_person_info(Person);
		
		_ ->
			"<error>" ++ Args ++ "</error>"
	end.


%%  P E O P L E

%% Get Swarmers around a position
get_people_around(Lat, Lgn, Offline) ->
	Rows = swarm_mnesia:do(qlc:q([X || X <- mnesia:table(person)])),
	PeopleXML = people_to_xml(Rows).


%% Get Favorite Swarmers
get_favorite_people(p_id) ->
	Rows = swarm_mnesia:do(qlc:q([X || X <- mnesia:table(person)])),
	PeopleXML = people_to_xml(Rows).


people_to_xml(Rows) ->
	%% Create People XML
	{RootEl, _} = xmerl_scan:string("<people/>"),
	#xmlElement{content = Content} = RootEl,
	
	%% For each Person Row
	NewContent = export_person(Rows, Content, 0),
	lists:flatten([NewContent]),
	%%io:format("people xml : ~p~n", [NewContent]),
	
	NewRootEl=RootEl#xmlElement{content=NewContent},
	PeopleXML=xmerl:export_simple([NewRootEl], xmerl_xml),
	lists:flatten(PeopleXML).
	%%io:format("people xml : ~p~n", [Export]),


%% Recursive person Export
export_person([], Data, Type) ->
	Data;
export_person([Row | NewRows], OldData, Type) ->
	case Type of
	0 -> %% people around / favorites
	Data = [{person, [], [
			{id, [], ["1"]},%%Row#person.id},
			{name, [], [Row#person.name]},
			{online, [], ["true"]}, %%[Row#person.online]},
			{status, [], [Row#person.status]},
			{img_url, [], ["images/" ++ Row#person.image_url]}
		   ]}]
	end,
	NewData = OldData ++ Data,
	export_person(NewRows, NewData, Type).



%%  P E R S O N

%% Get Swarmer around a position
get_person_info(P_id) ->
	Fun = fun() -> mnesia:read({person, P_id}) end,
	{atomic, [Row]} = mnesia:transaction(Fun),
	
	Data = [{person, [], [
			{id, [], [Row#person.id]},
			{name, [], [Row#person.name]},
			{status, [], [Row#person.status]}
		   ]}],

	NewRootEl=#xmlElement{content=Data},
	PersonXML=xmerl:export_simple([NewRootEl], xmerl_xml),
	lists:flatten(PersonXML).


%% Create Swarmer
create_person(Id, Name, Online, Status, ImageURL) ->
	Row = #person{id=Id, name=Name, password="", online=Online, status=Status, image_url=ImageURL, bio="", website="", karma=32, location="", event = ""},
	F = fun() -> mnesia:write(Row) end,
	mnesia:transaction(F).


%% Insert fake users in Mnesia!
insert_test_data() ->
	io:format("SWARM: Adding Fake Users~n"),
	create_person(0,"Superman", true, "Flying...", "superman_48.png"),
	create_person(1,"Spiderman", false, "Climbing...", "spiderman_48.png"),
	create_person(2,"Aquaman", false, "Swimming...", "aquaman_48.png").
	%% p_at_pos, {person, lat, lgn}).


fake_people() -> 
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


%%	- (NSString *)createPerson:(Person*)person;
%%	- (NSString *)flagPerson:(uint)personID :(NSString *)flag;
%%	- (NSString *)votePerson:(uint)personID :(NSString *)vote;
%%	- (NSString *)editPerson:(uint)personID :(Person*)person;
%%	- (NSString *)removePerson:(uint)personID;
%%- (NSString *)sendPersonImage:(uint)personID :(NSString*)image;
