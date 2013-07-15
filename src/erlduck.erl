-module(erlduck).
-include("erlduck.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% User functions
-export([search/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Protocol} = application:get_env(protocol),
    {ok, Host} = application:get_env(host),
    {ok, Port} = application:get_env(port),
    DDG_Service = #erlduck_ddg {
        protocol = Protocol,
        host = Host,
        port = Port
    },
    erlduck_sup:start_link(DDG_Service).

stop(_State) ->
    ok.

%% ===================================================================
%% User functions
%% ===================================================================

search(Term) ->
    {ok, Pid} = supervisor:start_child(erlduck_sup, []),
    Result = erlduck_server:search(Pid, Term),
    supervisor:terminate_child(erlduck_sup, Pid),
    Result.

