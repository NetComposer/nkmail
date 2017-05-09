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


login() ->
    nkdomain_sample:login().


send1() ->
    Msg = #{
        provider => "info.netcomposer",
        % from => "My test <info.netcomposer@gmail.com>",
        to => "My dest <carlosj.gf@gmail.com>, <listas1@maycar.net>",
        subject => "sub2",
        body => "msg2"
    },
    nkdomain_sample:cmd(nkmail, send, Msg).



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
    nkdomain_sample:cmd(nkmail, send, Msg).


send3() ->
    Msg = #{
        provider => "direct",
        to => "<carlosj.gf@gmail.com>",
        subject => "sub2",
        body => "msg2"
    },
    nkdomain_sample:cmd(nkmail, send, Msg).



%%% DOMAIN

prov_create() ->
    Data = #{
        obj_name => prov1,
        'mail.config' => #{
            class => smtp,
            from => "test@test.com",
            config => #{
                relay => relay
            }
        }
    },
    nkdomain_sample:cmd('mail.config', create, Data).








