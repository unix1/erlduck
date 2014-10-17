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
    ok = application:load(erlduck),
    [application:set_env(App, Key, Val) || {App, Key, Val} <-
     [{erlduck, connections,
       [{default,
         [{protocol, "https"},
          {host, "api.duckduckgo.com"},
          {port, 443},
          {user, <<>>},
          {pass, <<>>},
          {pool, erlduck_test_pool},
          {http_backend, httpclient_http_gun}, % backend for login service
          {login_handler, erlduck_login}, % authentication implementation
          {service_handler, erlduck_server} % service implementation
          ]}]},
      {erlduck, service_pools,
       [{erlduck_test_pool,
         [ % size args
           {size, 1}, % max pool size
           {max_overflow, 0}], % max # of workers created if pool is empty
         [ % worker args
           {connection, default},
           {http_backend, httpclient_http_gun}]}] % backend for service workers
       }]
    ],
    ok = erlduck:start(),
    Config.

end_per_suite(_) ->
    ok = erlduck:stop(),
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
    true = is_binary(proplists:get_value(<<"Answer">>, Result)).

get_answer_named(_) ->
    Result = erlduck:get_answer(default, <<"duckduckgo">>),
    true = is_binary(proplists:get_value(<<"Answer">>, Result)).

search(_) ->
    {ok, search_results, _} = erlduck:search(<<"duckduckgo">>).

search_named(_) ->
    {ok, search_results, _} = erlduck:search(default, <<"duckduckgo">>).
