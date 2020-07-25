-module(erlduck_server).

-behavior(httpclient_service_handler).

%% User functions
-export([get_answer/2]).
-export([search/2]).

%% Behavior callbacks
-export([get_request/3]).

-type answer() :: jsx:json_term().

%% ============================================================================
%% User functions
%% ============================================================================

-spec get_answer(atom(), binary()) -> answer().
get_answer(ConnName, Question) ->
    {ok, search_results, AnswerRaw} = search(ConnName, Question),
    decode_response(AnswerRaw).

-spec search(atom(), binary()) -> {ok, search_results, answer()}.
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

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec decode_response(binary()) -> answer().
decode_response(RawResponse) ->
    jsx:decode(RawResponse, [return_maps]).
