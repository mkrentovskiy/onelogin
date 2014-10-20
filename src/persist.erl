-module(persist).

-export([init_db/1]).
-export([check_user/3, add_user/3, del_user/2]).

-include_lib("deps/alog/include/alog.hrl").

%
% init db ( persist:init_db(pgdb). )
%

init_db(Pool) ->
    persist_pgsql:ql(Pool, [
            %
            % Users
            %

            "CREATE TABLE users (
                    mail varchar(128) PRIMARY KEY,
                    pass varchar(64) NOT NULL,
                    en boolean NOT NULL DEFAULT TRUE,
                    atime timestamp DEFAULT current_timestamp
                );",
            "CREATE INDEX users_mail_pass_en_idx ON users(mail, pass, en);",
            %
            % Projects
            %
            "CREATE TABLE clients (
                    domain varchar(128),
                    key varchar(128),
                    en boolean NOT NULL DEFAULT TRUE,
                    atime timestamp DEFAULT current_timestamp
                );",
            "CREATE INDEX clients_domain_en_idx ON clients(domain, en);"
        ]).

%
% user's ops
%

check_user(Pool, Mail, Pass) ->
    case persist_pgsql:qe(Pool, "SELECT COUNT(mail) AS c FROM users WHERE en=TRUE AND mail=$1 AND pass=$2", [Mail, Pass]) of
        {ok, {_, [{Count}]}} when Count =:= 1 ->
            ?INFO("Auth for user ~p successful.", [Mail]), 
            true;
        Any -> 
            ?WARNING("Auth for user ~p fail (pass ~p, reply ~p)", [Mail, Pass, Any]),
            false
    end.

add_user(Pool, Mail, Pass) ->
    persist_pgsql:qe(Pool, "INSERT INTO users(mail, pass) VALUES ($1, $2);", [Mail, Pass]).

del_user(Pool, Mail) ->
    persist_pgsql:qe(Pool, "UPDATE users SET en=FALSE WHERE mail=$1;", [Mail]).

%
% domain's ops
%


