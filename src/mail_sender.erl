-module(mail_sender).
-behaviour(gen_server).

-export([mail/3]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("onelogin.hrl").
-include_lib("deps/alog/include/alog.hrl").

mail(To, Template, Params) ->
    gen_server:cast(?MODULE, {mail, To, Template, Params}).

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(Params) ->
    Priv = onelogin_app:priv_dir(),
    lists:map(fun({Fn, Module}) -> 
            File = lists:concat([Priv, "/templates/", Fn, ".dtl"]),
            {ok, _} = erlydtl:compile_file(File, Module, [{auto_escape, false}])
        end, ?TEMPLATES),
    From = proplists:get_value(username, Params),
    {ok, #mails{ from = From, relay = Params }}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast({mail, To, Template, Params}, State) ->
    Last = case lists:filter(fun(I) -> I =:= To end, State#mails.last_mails) of
        [] ->
            {ok, UBody} = Template:render(Params ++ [{mail_to, To}, {domain, ?CONFIG(domain, "localhost")}]),
            Body = binary_to_list(iolist_to_binary(UBody)),
            ?DEBUG("Send ~p to ~p and relay ~p", [Body, To, State#mails.relay]),
            R = gen_smtp_client:send({ State#mails.from, [To], Body }, State#mails.relay),
            ?DEBUG("Mail client replay ~p", [R]),
            ?AFTER(?MAIL_RESEND_TIMEOUT, {reduce, To}),
            State#mails.last_mails ++ [To];
        _ ->
            ?ASYNC(fun() -> timer:sleep(?MAIL_RESEND_TIMEOUT), mail(To, Template, Params) end),
            State#mails.last_mails
    end, 
    {noreply, State#mails{ last_mails = Last }};

handle_cast(_Msg, State) -> 
    {noreply, State}.


handle_info({reduce, M}, State) -> 
    Last = lists:filter(fun(I) -> I =/= M end, State#mails.last_mails),
    {noreply, State#mails{ last_mails = Last }};

handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, _State) -> 
    ok.


code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

