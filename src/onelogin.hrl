%
% Common project options
%

-define(RECONNECT_TIMEOUT, ?CONFIG("reconnect_timeout", 5000)).

-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).
-define(ASYNC(F), proc_lib:spawn(fun() -> F end)).

-define(CONFIG(Key, Default), onelogin_app:config(Key, Default)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 5000, Type, [I]}).

-define(ME(Reg), gproc:reg({n, l, Reg})).
-define(LOOKUP(Reg), gproc:lookup_pid({n, l, Reg})).
-define(LOOKUPS(Reg), gproc:lookup_pids({n, l, Reg})).
-define(PUB(Event, Msg), pubsub:pub(Event, Msg)).
-define(SUB(Event), pubsub:sub(Event)).
-define(UNSUB(Event), pubsub:unsub(Event)).

%
% States
%

-record(mails, {
        
    }).


-define(SIDC, <<"ol_sid">>).
-record(session, {

    }).