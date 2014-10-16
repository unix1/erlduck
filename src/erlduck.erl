-module(erlduck).

-behaviour(application).

%% Behavior callbacks
-export([start/2]).
-export([stop/1]).

%% User functions
-export([start/0]).
-export([stop/0]).
-export([search/1, search/2]).

-define(DEFAULT_CONN, default).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    {ok, ConnectionsConfig} = application:get_env(connections),
    {ok, PoolConfig} = application:get_env(service_pools),
    httpclient:start_pool(ConnectionsConfig, PoolConfig),
    erlduck_sup:start_link().

stop(_State) ->
    ok.

%% ============================================================================
%% User functions
%% ============================================================================

start() ->
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(inets),
    ok = application:ensure_started(poolboy),
    ok = application:ensure_started(gun),
    ok = application:ensure_started(httpclient),
    ok = application:ensure_started(erlduck),
    ok.

stop() ->
    ok = application:stop(erlduck),
    ok = application:stop(httpclient),
    ok = application:stop(gun),
    ok = application:stop(poolboy),
    ok = application:stop(inets),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(asn1),
    ok = application:stop(cowlib),
    ok = application:stop(crypto),
    ok = application:stop(ranch),
    ok.

search(Term) ->
    search(?DEFAULT_CONN, Term).

search(Connection, Term) ->
    erlduck_server:search(Connection, Term).
