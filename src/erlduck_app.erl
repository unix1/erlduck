-module(erlduck_app).

-behaviour(application).

%% Behavior callbacks
-export([start/2]).
-export([stop/1]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    erlduck_sup:start_link().

stop(_State) ->
    ok.
