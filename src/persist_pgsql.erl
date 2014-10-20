-module(persist_pgsql).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([q/2, ql/2, qs/2, qe/3, field/3, field/4]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("onelogin.hrl").

-include_lib("deps/alog/include/alog.hrl").
-include_lib("deps/epgsql/include/pgsql.hrl").

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link(?MODULE, Params, []).


init([Params]) ->
    try_pgsql(ok, Params).


q(Pool, Query) -> 
    poolboy:transaction(Pool, fun(Worker) ->
            gen_server:call(Worker, {q, Query})
        end).   
    

ql(Pool, QueryList) when is_list(QueryList) ->
    [q(Pool, Q) || Q <- QueryList]. 


qs(Pool, Query) ->
    case persist_pgsql:q(Pool, Query) of
        {ok, _} -> 
            ok;
        Error ->
            ?ERROR("Error ~p", [Error]),
            error
    end.


qe(Pool, Query, Params) -> 
    poolboy:transaction(Pool, fun(Worker) ->
            gen_server:call(Worker, {qe, Query, Params})
        end).   


field(Row, Column, Columns) ->
    ColumnB = list_to_binary(Column),
    lists:filter(fun({C, _}) -> C#column.name == ColumnB end, 
        lists:zip(Columns, tuple_to_list(Row))).


field(Row, Column, Columns, Value) ->
    ValueB = list_to_binary(Value),
    case field(Row, Column, Columns) of
        [{_, ValueB}] ->
            true;
        _ -> 
            false
    end.

%
% gen_server
%

handle_call({q, _Query}, _From, undefined) -> 
    {reply, {error, noconnection}, undefined};

handle_call({q, Query}, _From, C) -> 
    case pgsql:squery(C, Query) of
        {ok, Count} ->
            {reply, {ok, Count}, C};
        {ok, Columns, Rows} ->
            {reply, {ok, {Columns, Rows}}, C};
        {ok, _Count, Columns, Rows} ->
            {reply, {ok, {Columns, Rows}}, C};
        Err ->
            ?ERROR("PostgreSQL error ~p - ~p", [Query, Err]),
            {reply, {error, Err}, C}
    end;

handle_call({qe, Query, Params}, _From, C) -> 
    case pgsql:equery(C, Query, Params) of
        {ok, Count} ->
            {reply, {ok, Count}, C};
        {ok, Columns, Rows} ->
            {reply, {ok, {Columns, Rows}}, C};
        {ok, _Count, Columns, Rows} ->
            {reply, {ok, {Columns, Rows}}, C};
        Err ->
            ?ERROR("PostgreSQL error ~p - ~p", [Query, Err]),
            {reply, {error, Err}, C}
    end;

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast(_Msg, State) -> 
    {noreply, State}.


handle_info({reinit, Params}, undefined) -> 
    try_pgsql(noreply, Params);

handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, undefined) -> 
    ok;

terminate(_Reason, C) -> 
    ?INFO("PgSQL connection down ~p", [C]),
    pgsql:close(C),
    ok.


code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%
% misc
%

try_pgsql(RAtom, []) ->
    ?INFO("PgSQL not started, skip."),
    {RAtom, undefined};

try_pgsql(RAtom, Params) ->
    case init_pgsql(Params) of
        {ok, RPid} ->
            ?INFO("PgSQL connection up ~p", [RPid]),
            {RAtom, RPid};
        Reason ->
            ?ERROR("Error in connection to PgSQL server - ~p", [Reason]),
            ?AFTER(?RECONNECT_TIMEOUT, {reinit, Params}),
            {RAtom, undefined}
    end.


init_pgsql(Params) -> 
    Host = proplists:get_value(host, Params),
    User = proplists:get_value(user, Params),
    Password = proplists:get_value(password, Params),
    Database = proplists:get_value(db, Params),
    pgsql:connect(Host, User, Password, [{database, Database}]).
