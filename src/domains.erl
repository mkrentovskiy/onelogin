-module(domains).
-behaviour(gen_server).

-export([check/3, sign/2, update/3, disable/1]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").


check(Domain, Data, Sign) ->
    case gen_server:call(?MODULE, {keypair, Domain}) of
        undefined ->
            ?INFO("Call for domain ~p keys - not found.", [Domain]),
            undefined;
        {DomainKey, ServerKey} ->
            LSign = sign(Data, DomainKey),
            case Sign =:= LSign of 
                true ->
                    ServerKey;
                false ->
                    ?WARNING("Fail signature check for domain ~p (want ~p, got ~p)", [Domain, LSign, Sign]),
                    undefined
            end
    end.


sign(Data, Key) ->
    list_to_binary(session:smd5(<<Data/binary, Key/binary>>)).


update(Domain, DomainKey, ServerKey) ->
    gen_server:call(?MODULE, {update, Domain, DomainKey, ServerKey}).


disable(Domain) ->
    gen_server:call(?MODULE, {disable, Domain}).

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(_) ->
    {ok, []}.

%
% gen_server
%

handle_call({keypair, Domain}, _From, State) ->
    case proplists:get_value(Domain, State, undefined) of
        undefined ->
            case persist:domain_keys(pgdb, Domain) of
                undefined -> 
                    {reply, undefined, State};
                Keys ->
                    {reply, Keys, State ++ [{Domain, Keys}]}
            end;
        Keys ->
            {reply, Keys, State}
    end;

handle_call({update, Domain, DomainKey, ServerKey}, _From, State) ->
    NewState = lists:filter(fun({K, _}) -> K =/= Domain end, State),
    Reply = persist:update_domain(pgdb, Domain, DomainKey, ServerKey),
    {reply, Reply, NewState};

handle_call({disable, Domain}, _From, State) ->
    NewState = lists:filter(fun({K, _}) -> K =/= Domain end, State),
    Reply = persist:disable_domain(pgdb, Domain),
    {reply, Reply, NewState};

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

