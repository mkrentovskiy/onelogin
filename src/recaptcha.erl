-module(recaptcha).

-export([check/2]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").

-define(HOST, "www.google.com").
-define(URL, "http://www.google.com/recaptcha/api/verify").


check(IP, Values) ->
    Challenge = proplists:get_value(<<"recaptcha_challenge_field">>, Values, <<"">>),
    Response = proplists:get_value(<<"recaptcha_response_field">>, Values, <<"">>),
    case (byte_size(Challenge) =:= 0) or (byte_size(Response) =:= 0) of
        true ->
            ?DEBUG("One or more params of Recaptcha are zero length (~p | ~p)", [Challenge, Response]),
            false;
        false ->
            api_call(?RECAPTCHA_KEY, IP, Challenge, Response)
    end.


api_call(Key, IP, Challenge, Response) ->
    Body = << 
            <<"privatekey=">>/binary, 
            Key/binary, 
            <<"&remoteip=">>/binary, 
            IP/binary, 
            <<"&challenge=">>/binary, 
            Challenge/binary, 
            <<"&response=">>/binary, 
            Response/binary
        >>,
    case ibrowse:send_req(?URL, [
                    {"Content-Type", "application/x-www-form-urlencoded;"},                    
                    {"Content-Length", byte_size(Body)}
                ], post, Body) of
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
