-module(auth_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).
-export([add/2, del/1, check/2]).

-include("onelogin.hrl").

-include_lib("deps/alog/include/alog.hrl").


init({tcp, http}, Req, _Opts) ->    
    {ok, Req, undefined_state}.


handle(Req, State) ->
    {ok, Req, State}.


terminate(_Reason, _Req, _State) ->
    ok.

%
% misc
%

add(Mail, Pwd) ->
    persist:add_user(pgdb, Mail, plain_pwd_to_db_pwd(Pwd)).


del(Mail) ->
    persist:del_user(pgdb, Mail).


check(Mail, MD5Pwd) ->
    persist:check_user(pgdb, Mail, pwd_to_db_pwd(MD5Pwd)).

%
% local
%

pwd_to_db_pwd(MD5Bin) ->
    Salt = ?CONFIG(salt, <<"9hH9typKseHMnBSbeFQscO0TaByUP06i">>),
    smd5(<<MD5Bin/binary, Salt/binary>>).


plain_pwd_to_db_pwd(Bin) when is_binary(Bin) ->
    plain_pwd_to_db_pwd(binary_to_list(Bin));

plain_pwd_to_db_pwd(L) ->
    pwd_to_db_pwd(list_to_binary(smd5(L))).


smd5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(S)]).


random() ->
    base64:encode(crypto:strong_rand_bytes(?CONFIG(sid_size, 64))).

