%
% Common project options
%

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
-define(LOOKUP_SUB(Reg), gproc:lookup_pid({p, l, Reg})).

-define(REAL_IP(Req), cowboy_req:header(<<"X-Real-IP">>, Req)).

-define(RE_MAIL, "^.+@[^@]+\\.[^@]{2,}$").
-define(RE_TOKEN, "^[a-zA-Z0-9=]+$").

%
% Params
%

-define(RECONNECT_TIMEOUT, 5 * 1000).
-define(MAIL_RESEND_TIMEOUT, 60 * 1000).
-define(MAX_FAIL_COUNT, 16).

-define(SHORT_SESSION, 30 * 60).
-define(MEDIUM_SESSION, 24 * 3600).
-define(LONG_SESSION, 30 * 24 * 3600).

-define(S2MS(S), S * 1000).

%
% States
%

-define(TEMPLATES, [
        {"reg", template_reg},
        {"reset", template_reset}
    ]).
-record(mails, {
        relay = [],
        from = "",
        last_mails = []
    }).

-define(SIDC, <<"ol_sid">>).
-record(session, {
        sid = undefined,
        time_to_die = undefined,
        timer = 0,
        auth = false,
        mail = undefined,
        info = [],
        fail_count = 0,
        token = <<"">>
    }).