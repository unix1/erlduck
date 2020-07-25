-module(erlduck).

%% User functions
-export([start/0]).
-export([start/1]).
-export([get_answer/1]).
-export([get_answer/2]).
-export([search/1]).
-export([search/2]).

-type connection_settings() :: #{
    name := atom(),
    pool_size := non_neg_integer(),
    pool_overflow := non_neg_integer()
}.

-type connection_options() :: #{
    name := atom(),
    pool_size := non_neg_integer(),
    pool_overflow := non_neg_integer()
}.

-type question() :: binary().
-type answer() :: erlduck_server:answer().

-export_type([question/0]).
-export_type([answer/0]).

-define(DEFAULT_CONN_NAME, default).
-define(DEFAULT_POOL_SIZE, 1).
-define(DEFAULT_POOL_OVERFLOW, 0).

%% ============================================================================
%% User functions
%% ============================================================================

-spec start() -> ok.
start() ->
    start_with_settings(default_settings()).

-spec start(connection_options()) -> ok.
start(Options) when is_map(Options) ->
    Name = maps:get(name, Options, ?DEFAULT_CONN_NAME),
    PoolSize = maps:get(pool_size, Options, ?DEFAULT_POOL_SIZE),
    PoolOverflow = maps:get(pool_overflow, Options, ?DEFAULT_POOL_OVERFLOW),
    start_with_settings(#{name => Name, pool_size => PoolSize, pool_overflow => PoolOverflow}).

-spec get_answer(question()) -> answer().
get_answer(Question) when is_binary(Question) ->
    get_answer(?DEFAULT_CONN_NAME, Question).

-spec get_answer(atom(), question()) -> answer().
get_answer(Connection, Question) when is_atom(Connection), is_binary(Question) ->
    erlduck_server:get_answer(Connection, Question).

-spec search(question()) -> {ok, search_results, binary()}.
search(Term) when is_binary(Term) ->
    search(?DEFAULT_CONN_NAME, Term).

-spec search(atom(), question()) -> {ok, search_results, binary()}.
search(Connection, Term) when is_atom(Connection), is_binary(Term) ->
    erlduck_server:search(Connection, Term).

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec start_with_settings(connection_settings()) -> ok.
start_with_settings(#{name := Name, pool_size := PoolSize, pool_overflow := PoolOverflow}) ->
    ConnectionsConfig = create_config_connections(Name),
    PoolConfig = create_config_service_pools(Name, PoolSize, PoolOverflow),
    httpclient:start_pool(ConnectionsConfig, PoolConfig).

-spec default_settings() -> connection_settings().
default_settings() ->
    #{
        name => ?DEFAULT_CONN_NAME,
        pool_size => ?DEFAULT_POOL_SIZE,
        pool_overflow => ?DEFAULT_POOL_OVERFLOW
    }.

-spec pool_name(atom()) -> atom().
pool_name(Name) when is_atom(Name) ->
    list_to_atom("erlduck_" ++ atom_to_list(Name)).

create_config_connections(Name) ->
    PoolName = pool_name(Name),
    [{Name,
        [{protocol, "https"},
         {host, "api.duckduckgo.com"},
         {port, 443},
         {user, <<>>},
         {pass, <<>>},
         {pool, PoolName}, % service pool for this connection
         {http_backend, httpclient_http_gun}, % backend for login service
         {http_backend_options, #{retry_timeout => 3000,
                                  http_opts => #{keepalive => 3000}}},
         {login_handler, erlduck_login}, % authentication implementation
         {service_handler, erlduck_server} % service implementation
        ]
    }].

create_config_service_pools(Name, PoolSize, PoolOverflow) ->
    PoolName = pool_name(Name),
    [{PoolName,
        [ % size args
          {size, PoolSize}, % max pool size
          {max_overflow, PoolOverflow}], % max # of workers created if pool is empty
        [ % worker args
          {connection, Name}, % specifies which connection this pool maps to
          {http_backend, httpclient_http_gun}, % backend for service workers
          {http_backend_options, #{retry_timeout => 3000,
                                   http_opts => #{keepalive => 3000}}}
        ]
    }].
