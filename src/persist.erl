-module(persist).

-export([init_db/1]).
-export([check_user/2, get_user/3, add_user/4, update_user/3, del_user/2]).
-export([domain_keys/2, update_domain/4, disable_domain/2]).

-include_lib("deps/alog/include/alog.hrl").

%
% init db ( persist:init_db(pgdb). )
%

init_db(Pool) ->
    persist_pgsql:ql(Pool, [
            %
            % Users
            %

            "CREATE TABLE ol_users (
                    mail varchar(128) PRIMARY KEY,
                    pass varchar(64) NOT NULL,
                    name varchar(128),
                    en boolean NOT NULL DEFAULT TRUE,
                    atime timestamp DEFAULT current_timestamp
                );",
            "CREATE INDEX ol_users_mail_en_idx ON ol_users(mail, en);",
            "CREATE INDEX ol_users_mail_pass_en_idx ON ol_users(mail, pass, en);",
            %
            % Projects
            %
            "CREATE TABLE ol_clients (
                    domain varchar(128),
                    domain_key varchar(128),
                    server_key varchar(128),
                    en boolean NOT NULL DEFAULT TRUE,
                    atime timestamp DEFAULT current_timestamp
                );",
            "CREATE INDEX ol_clients_domain_en_idx ON ol_clients(domain, en);"
        ]).

%
% user's ops
%

check_user(Pool, Mail) ->
    case persist_pgsql:qe(Pool, "SELECT COUNT(mail) AS c FROM ol_users WHERE en=TRUE AND mail=$1", [Mail]) of
        {ok, {_, [{Count}]}} when Count =:= 1 ->
            ?INFO("Check for user ~p successful.", [Mail]), 
            true;
        Any -> 
            ?WARNING("Check for user ~p fail (reply ~p)", [Mail, Any]),
            false
    end.


get_user(Pool, Mail, Pass) ->
    case persist_pgsql:qe(Pool, "SELECT mail, name FROM ol_users WHERE en=TRUE AND mail=$1 AND pass=$2", [Mail, Pass]) of
        {ok, {_, [{Mail, Name}]}} ->
            ?INFO("Auth for user ~p successful.", [Mail]), 
            {Mail, Name};
        Any -> 
            ?WARNING("Auth for user ~p fail (pass ~p, reply ~p)", [Mail, Pass, Any]),
            none
    end.


add_user(Pool, Mail, Name, Pass) ->
    persist_pgsql:qe(Pool, "INSERT INTO ol_users(mail, name, pass) VALUES ($1, $2, $3);", [Mail, Name, Pass]).


update_user(Pool, Mail, Pass) ->
    persist_pgsql:qe(Pool, "UPDATE ol_users SET pass=$1 WHERE mail=$2;", [Pass, Mail]).


del_user(Pool, Mail) ->
    persist_pgsql:qe(Pool, "UPDATE ol_users SET en=FALSE WHERE mail=$1;", [Mail]).

%
% domain's ops
%

domain_keys(Pool, Domain) ->
    case persist_pgsql:qe(Pool, "SELECT domain_key, server_key FROM ol_clients WHERE en=TRUE AND domain=$1;", [Domain]) of
        {ok, {_, [Keys]}} -> Keys;
        _Any -> undefined
    end.


update_domain(Pool, Domain, DomainKey, ServerKey) ->
    case persist_pgsql:qe(Pool, "UPDATE ol_clients SET domain_key=$1,server_key=$2 WHERE domain=$3;", 
            [DomainKey, ServerKey, Domain]) of
        {ok, 1} -> 
            ok;
        Any -> 
            ?WARNING("Got ~p on updating domain keys, make insert.", [Any]),
            case persist_pgsql:qe(Pool, "INSERT INTO ol_clients(domain, domain_key, server_key) VALUES ($1, $2, $3);", 
                    [Domain, DomainKey, ServerKey]) of
                {ok, 1} ->
                    ok;
                Any1 ->
                    ?ERROR("Got error ~p on updating domain keys.", [Any1]),
                    error
            end
    end.    


disable_domain(Pool, Domain) ->
    persist_pgsql:qe(Pool, "UPDATE ol_clients SET en=FALSE WHERE domain=$1;", [Domain]).
