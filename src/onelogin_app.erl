-module(onelogin_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([config/2, priv_dir/0]).

start(_StartType, _StartArgs) ->
    VRoutes = [
        {"/", cowboy_static, [
                {directory,  <<"priv/www">>},
                {file, <<"index.html">>},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}} 
            ]},
        {<<"/auth/[...]">>, auth_handler, []},
        {<<"/[...]">>, cowboy_static, [
                {directory,  <<"priv/www">>},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}} 
            ]},
        {'_', notfound_handler, []}],
    Routes = [{'_',  VRoutes}], 
    Dispatch = cowboy_router:compile(Routes),
    cowboy:start_http(webapp_http_listener, 5, 
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),
    onelogin_sup:start_link().


stop(_State) ->
    ok.


config(Key, Default) ->
    case application:get_env(Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.


priv_dir() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    filename:join(filename:dirname(Ebin), "priv").
