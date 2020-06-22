-module(online_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([get_answer/1]).
-export([get_answer_named/1]).
-export([search/1]).
-export([search_named/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [get_answer,
     get_answer_named,
     search,
     search_named].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlduck),
    erlduck:start(),
    Config.

end_per_suite(_) ->
    ok = application:stop(erlduck),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

get_answer(_) ->
    Result = erlduck:get_answer(<<"duckduckgo">>),
    true = is_binary(maps:get(<<"Answer">>, Result)),
    true = is_binary(maps:get(<<"Abstract">>, Result)).

get_answer_named(_) ->
    Result = erlduck:get_answer(default, <<"duckduckgo">>),
    true = is_binary(maps:get(<<"Answer">>, Result)),
    true = is_binary(maps:get(<<"Abstract">>, Result)).

search(_) ->
    {ok, search_results, _} = erlduck:search(<<"duckduckgo">>).

search_named(_) ->
    {ok, search_results, _} = erlduck:search(default, <<"duckduckgo">>).
