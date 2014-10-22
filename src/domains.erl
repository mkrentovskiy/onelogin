-module(domains).
-behaviour(gen_server).

%-export([]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link(?MODULE, Params, []).


init(Params) ->
    {ok, []}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast(_Msg, State) -> 
    {noreply, State}.


handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, _State) -> 
    ok.


code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

