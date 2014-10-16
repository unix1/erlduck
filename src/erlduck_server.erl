-module(erlduck_server).

-behavior(httpclient_service_handler).

%% User functions
-export([search/2]).

%% Behavior callbacks
-export([get_request/3]).

%% ============================================================================
%% User functions
%% ============================================================================

search(ConnName, SearchTerm) ->
    {ok, 200, _, ResponseBody} =
        httpclient_service:request(ConnName, {search, [SearchTerm]}),
    {ok, search_results, ResponseBody}.

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

get_request(search, [SearchTerm], _Token) ->
    Headers = [],
    Path = <<"/">>,
    Params = [{<<"q">>, SearchTerm},
              {<<"format">>, <<"json">>},
              {<<"t">>, <<"erlduck">>}],
    {ok, httpclient_req:new(get, Headers, Path, Params)}.
