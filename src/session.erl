-module(session).
-behaviour(gen_server).

-export([check/1, process/3]).
-export([add/2, del/1, check/2, random/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").


check(SID) ->
    try ?LOOKUP(SID) of 
        Pid ->
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

pass(_Auth, Path, Req, State) ->
    ?INFO("Unknown request ~p for session ~p", [Path, State]),
    {[{result, error}, {error, unknown_request}], Req, State}.


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



