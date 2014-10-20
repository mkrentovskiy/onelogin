-module(onelogin).

-export([start/0]).


start() ->
    ok = appstart(crypto),
    ok = appstart(alog),
    ok = appstart(gproc),
    ok = appstart(ranch),
    ok = appstart(cowlib),
    ok = appstart(cowboy),
    ok = appstart(ibrowse),
    ok = appstart(onelogin).


appstart(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        Err -> 
            io:format("{start} Got error ~p on ~p ~n", [Err, App]),
            error
    end.
