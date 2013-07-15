-module(erlduck_server).
-include("erlduck.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2,
         search/2]).

%%%%% Supervision functions %%%%%

%% start without name
start_link(DDG_Service) ->
    gen_server:start_link(?MODULE, [DDG_Service], []).

%% init, nothing to do
init([DDG_Service]) ->
    {ok, DDG_Service}.

%%%%% User functions %%%%%

search(Pid, Term) ->
    gen_server:call(Pid, {search, Term}).

%%%%% Server functions %%%%%

handle_call({search, Term}, _From, S) ->
    case libsearch(
        Term,
        S#erlduck_ddg.protocol, S#erlduck_ddg.host, S#erlduck_ddg.port
    ) of
        {ok, Result} ->
            {reply, {ok, Result}, S};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%%% Library functions %%%%%

%% executes search
libsearch(Term, Protocol, Host, Port) ->
    URI = Protocol ++ "://" ++ Host ++ ":" ++ Port ++ "/?q=" ++ http_uri:encode(Term) ++ "&format=json&t=erlduck",
    {ok, {{_Version, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} = httpc:request(URI),
    {ok, ResponseBody}.

