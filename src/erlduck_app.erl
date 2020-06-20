-module(erlduck_app).

-behaviour(application).

%% Behavior callbacks
-export([start/2]).
-export([stop/1]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    ConnectionsConfig = get_config_connections(),
    PoolConfig = get_config_service_pools(),
    httpclient:start_pool(ConnectionsConfig, PoolConfig),
    erlduck_sup:start_link().

stop(_State) ->
    ok.

%% ============================================================================
%% Internal functions
%% ============================================================================

get_config_connections() ->
   [{default,
      [{protocol, "https"},
       {host, "api.duckduckgo.com"},
       {port, 443},
       {user, <<>>},
       {pass, <<>>},
       {pool, erlduck_default}, % service pool for this connection
       {http_backend, httpclient_http_gun}, % backend for login service
       {http_backend_options, #{retry_timeout => 3000,
                                http_opts => #{keepalive => 3000}}},
       {login_handler, erlduck_login}, % authentication implementation
       {service_handler, erlduck_server} % service implementation
       ]}].

get_config_service_pools() ->
   [{erlduck_default,
      [ % size args
        {size, 1}, % max pool size
        {max_overflow, 0}], % max # of workers created if pool is empty
      [ % worker args
        {connection, default}, % specifies which connection this pool maps to
        {http_backend, httpclient_http_gun},
        {http_backend_options, #{retry_timeout => 3000,
                                 http_opts => #{keepalive => 3000}}}
        ]}]. % backend for service workers
