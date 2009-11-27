%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc Web server for swarm.

-module(swarm_web).
-author('jrbedard <jrbedard@gmail.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
	io:format("Request from '' : ~p~n", [Path]),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "people/" ++ Args ->
					Xml = swarm_people:request(Args),
					Req:ok({"text/xml", Xml});
				
				"locations/" ++ Args ->
					Xml = swarm_locations:request(Args),
					Req:ok({"text/xml", Xml});
				
				"events/" ++ Args ->
					Xml = swarm_events:request(Args),
					Req:ok({"text/xml", Xml});
				
				"maps" ++ Args ->
					Req:serve_file("map.html", DocRoot);
					%%MapOut = swarm_maps:request(Args),
					%%Req:ok({"text/html", MapOut});
				
				"messages/" ->
					Req:serve_file(Path, DocRoot);
				
				"images/" ++ ImgPath ->
					Req:serve_file(ImgPath, DocRoot);
						
				_ ->
					Req:ok({"text/plain", io_lib:format("test: ~p~n", [Path])})
                    %%Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
