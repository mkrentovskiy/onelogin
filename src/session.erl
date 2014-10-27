-module(session).
-behaviour(gen_server).

-export([check/1, process/3, random/0, pwd_to_db_pwd/1, smd5/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").


check(SID) ->
    try ?LOOKUP(SID) of 
        Pid ->
            ?DEBUG("Find pid - ~p", [Pid]),
            Timeout = gen_server:call(Pid, timer),
            {Pid, {SID, Timeout}}    
    catch 
        error:_ ->
            NewSID = random(),
            {ok, Pid} = start([NewSID]),
            {Pid, {NewSID, ?SHORT_SESSION}}
    end.


process(Pid, Path, Req) ->
    gen_server:call(Pid, {process, Path, Req}).

%
% external
%
 
start(Params) -> 
    ?DEBUG("Create session with ~p", [Params]),
    gen_server:start(?MODULE, Params, []).


init([SID]) ->
    ?ME(SID),
    {ok, Timer} = timer:kill_after(?S2MS(?SHORT_SESSION)),
    {ok, #session{ sid = SID, time_to_die = Timer, timer = ?SHORT_SESSION }}.

%
% gen_server
%

handle_call(info, _From, State) ->
    {ok, cancel} = timer:cancel(State#session.time_to_die),
    {ok, Timer} = timer:kill_after(?S2MS(State#session.timer)),    
    {reply, {State#session.mail, State#session.info}, State#session{ time_to_die = Timer }};

handle_call(timer, _From, State) ->
    {ok, cancel} = timer:cancel(State#session.time_to_die),
    {ok, Timer} = timer:kill_after(?S2MS(State#session.timer)),    
    {reply, State#session.timer, State#session{ time_to_die = Timer }};
    

handle_call({process, Path, Req}, _From, State) ->
    {ok, cancel} = timer:cancel(State#session.time_to_die),
    {Reply, NewReq, NewState} = pass(State#session.auth, Path, Req, State),
    {ok, Timer} = timer:kill_after(?S2MS(NewState#session.timer)),    
    {reply, {Reply, NewReq}, NewState#session{ time_to_die = Timer }};

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

%
% pass
%

-define(ER(Error), [{result, error}, {error, Error}]).

pass(false, [<<"user">>, <<"info">>], Req, State) ->
    {?ER(unknown_user), Req, State};

pass(false, [<<"login">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Mail = proplists:get_value(<<"mail">>, Values, <<"">>),
            Pwd = proplists:get_value(<<"password">>, Values, <<"">>),
            DBPwd = plain_pwd_to_db_pwd(Pwd),
            case persist:get_user(pgdb, Mail, DBPwd) of
                {M, N} ->
                    ?SUB(M),
                    {[{result, ok}], Req1, State#session{ auth = true, mail = M, info = [{name, N}], timer = ?LONG_SESSION }};
                _ ->
                    FC = State#session.fail_count + 1,
                    case FC > ?MAX_FAIL_COUNT of
                        true ->
                            {IP, Req2} = ?REAL_IP(Req1), 
                            ?INFO("User with IP ~p is look like c00l hAtzker! ~pth auth try.", [IP, FC]),
                            {?ER(unknown_user_really), Req2, State#session{ fail_count = FC }};
                        false ->
                            {?ER(unknown_user), Req1, State#session{ fail_count = FC }}
                    end
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

pass(false, [<<"reg">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Mail = proplists:get_value(<<"mail">>, Values, <<"">>),
            Name = proplists:get_value(<<"name">>, Values, <<"">>),
            case (re:run(Mail, ?RE_MAIL) =:= nomatch) or (byte_size(Name) =:= 0) of
                true ->
                    {?ER(bad_income), Req1, State};
                false ->
                    {IP, Req2} = ?REAL_IP(Req),
                    case recaptcha:check(IP, Values) of
                        false ->
                            {?ER(captcha_fail), Req2, State};
                        true ->
                            case persist:check_user(pgdb, Mail) of
                                true ->
                                    {?ER(already), Req2, State};    
                                false ->
                                    SMail = strip(Mail),
                                    SName = strip(Name),
                                    Token = random(),
                                    ?SUB(Token),
                                    mail_sender:mail(SMail, template_reg, [{token, Token}]),
                                    {[{result, ok}], Req2, State#session{ mail = SMail, token = Token, info = [{name, SName}], 
                                        timer = ?MEDIUM_SESSION }}
                            end
                    end
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

        
pass(false, [<<"reset">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Mail = proplists:get_value(<<"mail">>, Values, <<"">>),
            case re:run(Mail, ?RE_MAIL) =:= nomatch of
                true ->
                    {?ER(bad_income), Req1, State};
                false ->
                    {IP, Req2} = ?REAL_IP(Req),
                    case recaptcha:check(IP, Values) of
                        false ->
                            {?ER(captcha_fail), Req2, State};
                        true ->
                            case persist:check_user(pgdb, Mail) of
                                true ->
                                    SMail = strip(Mail),
                                    Token = random(),
                                    ?SUB(Token),
                                    mail_sender:mail(SMail, template_reset, [{token, Token}]),
                                    {[{result, ok}], Req2, State#session{ mail = SMail, token = Token, timer = ?MEDIUM_SESSION }};
                                false ->
                                    {?ER(unknown_user), Req2, State}                         
                            end
                    end
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

pass(false, [<<"update">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Pwd = proplists:get_value(<<"password">>, Values, <<"">>),
            Token = proplists:get_value(<<"token">>, Values, <<"">>),    
            case (byte_size(Pwd) =:= 0) or (re:run(Token, ?RE_TOKEN) =:= nomatch) of
                true ->
                    {?ER(bad_income), Req1, State};
                false ->
                    try ?LOOKUP_SUB(Token) of 
                        [Pid] when Pid =:= self() ->
                            ?UNSUB(Token),
                            update_password(State#session.mail, State#session.info, Pwd, Req1, State);                            
                        [Pid] when Pid =/= self() ->
                            {Mail, Info} = gen_server:call(Pid, info),
                            timer:kill_after(0, Pid),
                            update_password(Mail, Info, Pwd, Req1, State);
                        Any ->
                            ?DEBUG("Invalid token ~p - reply ~p", [Token, Any]),
                            {?ER(invalid_token), Req1, State}                
                    catch 
                        error:Error ->
                            ?DEBUG("Invalid token - error ~p", [Error]),
                            {?ER(invalid_token), Req1, State}
                    end
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

pass(true, [<<"user">>, <<"info">>], Req, State) ->
    Token = random(),
    {[ 
            {result, ok},
            {mail, State#session.mail},
            {name, proplists:get_value(name, State#session.info, <<"">>)},
            {token, Token}
        ], Req, State#session{ token = Token }};

pass(true, [<<"update">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Pwd = proplists:get_value(<<"password">>, Values, <<"123">>),
            Token = proplists:get_value(<<"token">>, Values, <<"">>),    
            case Token =:= State#session.token of
                true ->
                    DBPwd = plain_pwd_to_db_pwd(Pwd),
                    case persist:update_user(pgdb, State#session.mail, DBPwd) of
                        {ok, 1} ->
                            {[{result, ok}], Req1, State#session{ token = <<"">> }};
                        _ ->
                            {?ER(operation_failed), Req1, State#session{ token = <<"">> }}
                    end;        
                false ->
                    {?ER(invalid_token), Req1, State#session{ token = <<"">> }}
            end;
                Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

pass(true, [<<"logout">>], Req, State) ->
    {[{result, ok}], Req, State#session{ auth = false, mail = undefinded, info = [], fail_count = 0, timer = ?SHORT_SESSION}};

pass(_Auth, Path, Req, State) ->
    ?INFO("Unknown request ~p for session ~p", [Path, State]),
    {?ER(unknown_request), Req, State}.


update_password(Mail, Info, Pwd, Req, State) ->
    case Mail of
        undefined ->
            {?ER(unknown_user), Req, State};
        _ ->
            DBPwd = plain_pwd_to_db_pwd(Pwd),  
            case persist:check_user(pgdb, Mail) of
                true ->
                    case persist:update_user(pgdb, Mail, DBPwd) of
                        {ok, 1} ->
                            case persist:get_user(pgdb, Mail, DBPwd) of
                                {M, N} ->
                                    ?SUB(M),
                                    {[{result, ok}], Req, State#session{ auth = true, mail = M, info = [{name, N}], 
                                        timer = ?LONG_SESSION }};
                                _ ->
                                    {?ER(operation_failed), Req, State#session{ token = <<"">> }}    
                            end;
                        _ ->
                            {?ER(operation_failed), Req, State#session{ token = <<"">> }}
                    end;
                false ->
                    Name = proplists:get_value(name, Info, <<"Guest">>),
                    case persist:add_user(pgdb, Mail, Name, DBPwd) of
                        {ok, 1} ->
                            ?SUB(Mail),
                            {[{result, ok}], Req, State#session{ auth = true, mail = Mail, info = Info, 
                                        timer = ?LONG_SESSION, token = <<"">> }};
                        _ ->
                            {?ER(operation_failed), Req, State#session{ token = <<"">> }}
                    end
            end
    end.

%
% local
%

strip(S) -> 
    re:replace(S, "[<>&/]+", "", [global,{return, binary}]).


pwd_to_db_pwd(MD5Bin) ->
    smd5(<<MD5Bin/binary, ?SALT/binary>>).


plain_pwd_to_db_pwd(Bin) when is_binary(Bin) ->
    plain_pwd_to_db_pwd(binary_to_list(Bin));

plain_pwd_to_db_pwd(L) ->
    pwd_to_db_pwd(list_to_binary(smd5(L))).


smd5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(S)]).


random() ->
    base64:encode(crypto:strong_rand_bytes(?CONFIG(sid_size, 64))).
