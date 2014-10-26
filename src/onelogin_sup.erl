-module(onelogin_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("onelogin.hrl").

-include_lib("deps/alog/include/alog.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Modules = lists:flatten([apply(M) || M <- ?CONFIG(modules, [])]),
    {ok, {{one_for_one, 500, 1000}, Modules}}.


apply({Module, ConfigName}) ->
    ?CHILD(Module, worker, [ConfigName]);

apply({pool, Module, Items}) ->
    [poolboy:child_spec(Pool, 
                    [{name, {local, Pool}},
                     {worker_module, Module},
                     {size, Size},
                     {max_overflow, 0}], [Params]) 
        || {Pool, Size, Params} <- Items];

apply(Module) ->
    ?CHILD(Module, worker).
