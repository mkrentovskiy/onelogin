%
% Common project options
%

-define(RECONNECT_TIMEOUT, 5000).

-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).
-define(ASYNC(F), proc_lib:spawn(fun() -> F end)).

-define(CONFIG(Key, Default), onelogin_app:config(Key, Default)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 5000, Type, [I]}).

%
% States
%

-record(mails, {
        
    }).