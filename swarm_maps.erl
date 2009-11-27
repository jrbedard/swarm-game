%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc TEMPLATE.

-module(swarm_maps).
-author('jrbedard <jrbedard@gmail.com>').

-export([request/1]).


request(Args) ->
	io:format("Map '' : ~p~n", [Args]),
	case Args of
        "around/lat=" ++ Lgn ->
			render_map(<<"37.46">>, <<"-122.26">>, <<"14">>);
		_ ->
			"<error>" ++ Args ++ "</error>"
	end.


render_map(Lgn, Lat, Zoom) ->
	 map:render().
