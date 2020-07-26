-module(erlduck).

%% User functions
-export([start/0]).
-export([start/1]).
-export([answer/1]).
-export([answer/2]).
-export([answer/3]).

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
-type answer() :: binary().
-type format() :: json | xml.

-export_type([question/0]).
-export_type([answer/0]).
-export_type([format/0]).

-define(DEFAULT_CONN_NAME, default).
-define(DEFAULT_FORMAT, json).
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

-spec answer(question()) -> {ok, answer()}.
answer(Question) when is_binary(Question) ->
    answer(?DEFAULT_CONN_NAME, Question, ?DEFAULT_FORMAT).

-spec answer(atom(), question() | format()) -> {ok, answer()}.
answer(Connection, Question) when is_atom(Connection), is_binary(Question) ->
    answer(Connection, Question, ?DEFAULT_FORMAT);
answer(Question, Format) when is_binary(Question), is_atom(Format) ->
    answer(?DEFAULT_CONN_NAME, Question, Format).

-spec answer(atom(), question(), format()) -> {ok, answer()}.
answer(Connection, Question, Format) when is_atom(Connection), is_binary(Question), is_atom(Format) ->
    erlduck_server:answer(Connection, Question, Format).

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
