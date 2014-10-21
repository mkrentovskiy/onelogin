-module(auth_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").


init({tcp, http}, Req, _Opts) ->
    {SID, Req2} = cowboy_req:cookie(?SIDC, Req, undefined),
    {NewSID, Pid, Expire} = session:check(SID),
    Req3 = cowboy_req:set_resp_cookie(?SIDC, NewSID, [{max_age, Expire}, {path, "/"}], Req2),
    {ok, Req3, Pid}.


handle(Req, Pid) ->
    {PathInfo, Req2} = cowboy_req:path_info(Req),
    {Reply, Req3} = process(PathInfo, Req2, Pid),
    JsonReply = jsonx:encode(Reply),
    {ok, Req4} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], JsonReply, Req3),
    {ok, Req4, Pid}.


terminate(_Reason, _Req, _State) ->
    ok.

%
% local
%

process(Path, Req, Pid) ->
    
    ?INFO("Call for process ~p", [Path]),
    {[{result, ok}], Req}.