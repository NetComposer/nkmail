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
-compile([export_all, nowarn_export_all]).

-include_lib("nkservice/include/nkservice.hrl").

-define(SRV, nkmail_test).
-define(PASS, <<>>).

%% @doc Starts the service
start() ->
    Spec = #{
        plugins => [nkmail_smtp_client],
        packages => [
            #{
                id => mail1,
                class => 'Mail',
                config => #{
                    backendClass => smtp,
                    relay => "smtp.gmail.com",
                    port => 587,
                    from => <<"NC <carlosj.gf@gmail.com">>,
                    username => "carlosj.gf@gmail.com",
                    password => ?PASS,
                    force_tls => true,
                    debug => true
                }
            }
        ],
        modules => [
            #{
                id => s1,
                class => luerl,
                code => s1(),
                debug => true
            }
        ]
    },
    nkservice:start(?SRV, Spec).


%% @doc Stops the service
stop() ->
    nkservice:stop(?SRV).


send() ->
    nkservice_luerl_instance:call({?SRV, s1, main}, [sendMail],
                                  [<<"carlosj.gf@gmail.com">>, <<"test">>]).


s1() -> <<"
    mailConfig = {
        backendClass = 'smtp',
        relay = 'smtp.gmail.com',
        port = 587,
        from = 'NC <carlosj.gf@gmail.com>',
        username = 'carlosj.gf@gmail.com',
        password = '',
        force_tls = true,
        debug = true
    }

    mail2 = startPackage('Mail', mailConfig)

    function sendMail(dest, body)
        return mail2.send({to=dest, body=body})
    end

">>.



send1() ->
    Msg = #{
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
        from => "My test <info.netcomposer@gmail.com>",
        to => "My dest <carlosj.gf@gmail.com>",
        subject => "sub3",
        content_type => "text/html",
        %%  body => "This is <strong>msg3</strong> øÿ áñ"
        body => "This is <strong>msg3</strong> øÿ áéíóúñ",
        attachments => [
            #{
                name => "File1.jpg",
                content_type => "image/jpeg",
                body => F1
            },
            #{
                name => "File2.pdf",
                content_type => "application/pdf",
                body => F2
            }
        ]
    },
    send(Msg).


send(Msg) ->
    nkmail:send(?SRV, mail1, Msg).

