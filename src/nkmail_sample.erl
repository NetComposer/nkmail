%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc NkMAIL
-module(nkmail_sample).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-compile(export_all).

-include_lib("nkservice/include/nkservice.hrl").

-define(SRV, nkmail_test).
-define(WS, "ws:all:9010/nkmail").


%% @doc Starts the service
start() ->
    Spec = #{
        callback => ?MODULE,
        api_server => ?WS
    },
    nkservice:start(?SRV, Spec).


%% @doc Stops the service
stop() ->
    nkservice:stop(?SRV).



login() ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        user => <<"test">>
    },
    {ok, _Reply, _Pid} = nkapi_client:start(?SRV, ?WS, Login, Fun, #{}, nkmail_test_login).



send1() ->
    Msg = #{
        provider => "info.netcomposer",
        % from => "My test <info.netcomposer@gmail.com>",
        to => "My dest <carlosj.gf@gmail.com>, <listas1@maycar.net>",
        subject => "sub2",
        body => "msg2"
    },
    send(Msg).



send2() ->
    Dir = code:priv_dir(nkmail),
    {ok, F1} = file:read_file(filename:join(Dir, "sample.jpg")),
    {ok, F2} = file:read_file(filename:join(Dir, "sample.pdf")),

    Msg = #{
        provider => "info.netcomposer",
        from => "My test <info.netcomposer@gmail.com>",
        to => "My dest <carlosj.gf@gmail.com>, <listas1@maycar.net>",
        subject => "sub3",
        content_type => "text/html",
        %%  body => "This is <strong>msg3</strong> øÿ áñ"
        body => "This is <strong>msg3</strong> øÿ áéíóúñ",
        attachments => [
            #{
                name => "File1",
                content_type => "image/jpeg",
                body => base64:encode(F1)
            },
            #{
                name => "File2",
                content_type => "application/pdf",
                body => base64:encode(F2)
            }
        ]
    },
    send(Msg).


send3() ->
    Msg = #{
        provider => "direct",
        to => "<carlosj.gf@gmail.com>",
        subject => "sub2",
        body => "msg2"
    },
    send(Msg).



%% ===================================================================
%% Util
%% ===================================================================


get_client() ->
    [{_, Pid}|_] = nkapi_client:get_all(),
    Pid.


send(Data) ->
    nkapi_client:cmd(get_client(), <<"nkmail/send">>, Data).


%% ===================================================================
%% Client fun
%% ===================================================================


api_client_fun(#nkreq{cmd = <<"event">>, data=Event}, State) ->
    lager:notice("CLIENT event ~p", [lager:pr(Event, nkevent)]),
    {ok, State};

api_client_fun(#nkreq{cmd = <<"nkapi_test_login">>, data=Data}, State) ->
    {ok, #{client=>Data}, State};

api_client_fun(_Req, State) ->
    % lager:error("API REQ: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, State}.



%% ===================================================================
%% API callbacks
%% ===================================================================

plugin_deps() ->
    [nkapi, nkmail, nkmail_smtp_client].



%% @doc
service_api_syntax(_Id, Syntax, #nkreq{cmd = <<"nkmail_test_login">>}=Req) ->
    {Syntax#{user=>binary}, Req};

service_api_syntax(_Id, _Syntax, _Req) ->
    continue.


%% @doc
service_api_allow(_Id, _Req) ->
    true.


%% @doc Called on any command
service_api_cmd(_Id, #nkreq{cmd = <<"nkmail_test_login">>, session_id=SessId, data=Data}=Req) ->
    case Data of
        #{user:=User} ->
            {ok, #{sess_id=>SessId}, Req#nkreq{user_id=User}};
        _ ->
            {error, invalid_user}
    end;

service_api_cmd(_Id, _Req) ->
    continue.