%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc Callbacks for the swarm application.

-module(swarm_app).
-author('jrbedard <jrbedard@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for swarm.
start(_Type, _StartArgs) ->
    swarm_deps:ensure(),
    swarm_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for swarm.
stop(_State) ->
    ok.
