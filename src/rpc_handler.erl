-module(rpc_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").


init({tcp, http}, Req, _Opts) ->
    {Path, Req1} = cowboy_req:path_info(Req),
    {Domain, Data, Sign, Values, Req2} = extract(Path, Req1),
    case domains:check(Domain, Data, Sign) of
        undefined ->
            {ok, Req2, undefined};
        ServerKey ->
            {ok, Req2, {Path, Values, ServerKey}}
    end.


handle(Req, undefined) ->
    {ok, Req1} = cowboy_req:reply(200, 
        [{<<"content-type">>, <<"application/json">>}], 
        <<"{'result': 'error', 'error': 'access_deny'}">>, 
        Req),
    {ok, Req1, undefined};

handle(Req, {Path, Values, ServerKey} = State) ->
    Reply = process(Path, Values, ServerKey),
    JsonReply = jsonx:encode(Reply),
    {ok, Req1} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], JsonReply, Req),
    {ok, Req1, State}.


terminate(_Reason, _Req, _State) ->
    ok.

%
% local
%

extract([<<"auth">>], Req) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Mail = proplists:get_value(<<"mail">>, Values, <<"">>),
            MD5Pwd = proplists:get_value(<<"password">>, Values, <<"">>),
            Sign = proplists:get_value(<<"sign">>, Values, <<"">>),
            Domain = proplists:get_value(<<"domain">>, Values, <<"">>),
            {Domain, <<Mail/binary, MD5Pwd/binary>>, Sign, Values, Req1};
        Any ->
            ?ERROR("Error with extract from POST request ~p", [Any]),
            {<<"localhost">>, <<"">>, <<"">>, [], Req}
    end;

extract(_Path, Req) ->
    {<<"localhost">>, <<"">>, <<"">>, [], Req}.


process([<<"auth">>], Values, ServerKey) ->
    Mail = proplists:get_value(<<"mail">>, Values, <<"">>),
    DBPwd = session:pwd_to_db_pwd(proplists:get_value(<<"password">>, Values, <<"">>)),
    case persist:get_user(pgdb, Mail, DBPwd) of 
        {M, N} -> 
            Sign = domains:sign(<<"ok", M/binary, N/binary>>, ServerKey),
            [{result, ok}, {mail, M}, {name, N}, {sign, Sign}];
        none ->
            Sign = domains:sign(<<"errornot_found">>, ServerKey),
            [{result, error}, {error, not_found}, {sign, Sign}]
    end;

process(Path, _Values, ServerKey) ->
    ?INFO("Call for process ~p", [Path]),
    Sign = domains:sign(<<"errorundefined_request">>, ServerKey),
    [{result, error}, {error, undefined_request}, {sign, Sign}].
