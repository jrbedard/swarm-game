%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc TEMPLATE.

-module(swarm).
-author('jrbedard <jrbedard@gmail.com>').
-export([start/0, stop/0]).

-include("swarm.hrl").


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the swarm server.
start() ->
    swarm_deps:ensure(),
    ensure_started(crypto),
    application:start(swarm),
	swarm_mnesia:start().

%% @spec stop() -> ok
%% @doc Stop the swarm server.
stop() ->
    Res = application:stop(swarm),
    application:stop(crypto),
	%%swarm_mnesia:stop(),
    Res.
													