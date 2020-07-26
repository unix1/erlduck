-module(erlduck_server).

-behavior(httpclient_service_handler).

%% User functions
-export([answer/3]).

%% Behavior callbacks
-export([get_request/3]).

-type search() :: #{
    term := erlduck:question(),
    format := erlduck:format()
}.

-define(DEFAULT_FORMAT, json).

%% ============================================================================
%% User functions
%% ============================================================================

-spec answer(atom(), binary(), erlduck:format()) -> {ok, erlduck:answer()}.
answer(ConnName, Term, Format) ->
    Search = #{
        term => Term,
        format => Format
    },
    {ok, 200, _, ResponseBody} =
        httpclient_service:request(ConnName, {search, [Search]}),
    {ok, ResponseBody}.

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

-spec get_request(atom(), [search()], term) -> {ok, term()}.
get_request(search, [#{term := Term, format := Format}], _Token) ->
    Headers = [],
    Path = <<"/">>,
    Params = [
        {<<"q">>, Term},
        {<<"format">>, response_format(Format)},
        {<<"t">>, <<"erlduck">>}
    ],
    {ok, httpclient_req:new(get, Headers, Path, Params)}.

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec response_format(erlduck:format()) -> binary().
response_format(json) -> <<"json">>;
response_format(xml) -> <<"xml">>.