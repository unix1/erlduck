-module(erlduck).

%% User functions
-export([get_answer/1]).
-export([get_answer/2]).
-export([search/1]).
-export([search/2]).

-define(DEFAULT_CONN, default).

%% ============================================================================
%% User functions
%% ============================================================================

get_answer(Question) ->
    get_answer(?DEFAULT_CONN, Question).

get_answer(Connection, Question) ->
    erlduck_server:get_answer(Connection, Question).

search(Term) ->
    search(?DEFAULT_CONN, Term).

search(Connection, Term) ->
    erlduck_server:search(Connection, Term).
