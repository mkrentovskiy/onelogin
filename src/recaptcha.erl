-module(recaptcha).

-export([check/1]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").

-define(URL, "http://www.google.com/recaptcha/api/verify").


check(Req) ->
    {{IP, _}, Req2} = cowboy_req:peer(Req),
    {Challenge, Req3} = cowboy_req:qs_val(<<"recaptcha_challenge_field">>, Req2, <<"">>),
    {Response, Req4} = cowboy_req:qs_val(<<"recaptcha_response_field">>, Req3, <<"">>),
    Key = ?CONFIG(recaptcha_private_key, ""),
    case (length(Challenge) == 0) or (length(Response) == 0) or (length(Key) == 0)  of
        true ->
            ?DEBUG("One or more params of Recaptcha are zero length (~p | ~p | ~p)", [Challenge, Response, Key]),
            {false, Req4};
        false ->
            Check = api_call(Key, IP, Challenge, Response),
            {Check, Req4}
    end.


api_call(Key, IP, Challenge, Response) ->
    Body = lists:concat(["privatekey=", Key, "&remoteip=", IP, "&challenge=", Challenge, "&response=", Response]),
    case ibrowse:send_req(?URL, [], post, Body) of
        {ok, "200", _, Data} ->
            ?DEBUG("Recaptcha reply: ~p", [Data]),
            case string:tokens(Data, "\n") of 
                ["true"] -> true;
                ["true", _] -> true;
                ["false", Reason] ->
                    ?ERROR("Recaptcha fail with the reason: ~p", [Reason]),
                    false;
                Any ->
                    ?ERROR("Unknown server reply: ~p", [Any]),
                    false
            end;
        Any ->
            ?INFO("Recaptcha fail with result: ~p", [Any]),
            false
    end.
