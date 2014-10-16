-module(erlduck_login).

-behavior(httpclient_login_handler).

-export([login/2]).

%% ============================================================================
%% Behavior functions
%% ============================================================================

login(_Conn, _HttpState) ->
    {ok, 0}.
