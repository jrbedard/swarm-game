%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc TEMPLATE.

-module(swarm_mnesia).
-author('jrbedard <jrbedard@gmail.com>').

-include_lib("stdlib/include/qlc.hrl").
-include("swarm_schema.hrl").

-export([do/1, start/0, stop/0]).


start() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	reset_table(),
	init(),
	%%mnesia:wait_for_tables([],90000),
	%% test data.
	insert_test_data().


insert_test_data() ->
	swarm_people:insert_test_data(),
	swarm_events:insert_test_data(),
	swarm_locations:insert_test_data().


stop() ->
	mnesia:stop().


reset_table() ->
	mnesia:clear_table(person),
	mnesia:clear_table(location),
	mnesia:clear_table(event).		


do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.


init() ->
	mnesia:create_table(person,
                        [{attributes, record_info(fields, person)},
						{ram_copies, [node()]},
						{type, bag}]),
						
    mnesia:create_table(location,
                         [{attributes, record_info(fields, location)},
						  {ram_copies, [node()]},
						  {type, bag}]),
							
    mnesia:create_table(event,
                         [{attributes, record_info(fields, event)},
						  {ram_copies, [node()]},
						  {type, bag}]),

	mnesia:create_table(p_at_loc, [{type, bag}, 
                       {ram_copies, [a@gin, b@skeppet]},
                       {attributes, record_info(fields,
                                                p_at_loc)}]).
