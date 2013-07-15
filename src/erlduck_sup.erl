-module(erlduck_sup).
-include("erlduck.hrl").
-export([start_link/1, init/1]).
-behaviour(supervisor).

start_link(DDG_Service) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [DDG_Service]).

init([DDG_Service]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    ChildSpec = {
        erlduck_server,
        {erlduck_server, start_link, [DDG_Service]},
        temporary,
        5000, % shutdown time
        worker,
        [erlduck_server]
    },
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.

