-module(online_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([answer/1]).
-export([answer_named/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        answer,
        answer_named
    ].

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

answer(_) ->
    {ok, Response} = erlduck:answer(<<"duckduckgo">>),
    Result = jsx:decode(Response, [return_maps]),
    true = is_binary(maps:get(<<"Answer">>, Result)),
    true = is_binary(maps:get(<<"Abstract">>, Result)).

answer_named(_) ->
    {ok, Response} = erlduck:answer(default, <<"duckduckgo">>),
    Result = jsx:decode(Response, [return_maps]),
    true = is_binary(maps:get(<<"Answer">>, Result)),
    true = is_binary(maps:get(<<"Abstract">>, Result)).
