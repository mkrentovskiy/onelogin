-module(rpc_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").


init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined}.


handle(Req, undefined) ->
    {ok, Req2} = cowboy_req:reply(200, 
        [{<<"content-type">>, <<"application/json">>}], 
        <<"{'result': 'error', 'error': 'unauth'}">>, 
        Req),
    {ok, Req2, undefined};

handle(Req, State) ->
    {PathInfo, Req2} = cowboy_req:path_info(Req),
    {Reply, Req3} = process(PathInfo, Req2, State),
    JsonReply = jsonx:encode(Reply),
    {ok, Req4} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], JsonReply, Req3),
    {ok, Req4, State}.


terminate(_Reason, _Req, _State) ->
    ok.

%
% local
%

process(Path, Req, State) ->
    ?INFO("Call for process ~p", [Path]),
    {[{result, ok}], Req}.