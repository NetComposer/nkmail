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

-module(nkmail).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([send/2]).
-export_type([mail_msg/0, provider_id/0, provider_class/0]).
-include("nkmail.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type provider_id() :: term().
-type provider_class() :: atom().

-type mail_msg() ::
    #{
        provider_id => provider_id(),
        from => binary(),           % Optional, can be "a@a.com" or "Name <a@a.com>"
        to => binary(),             % Can be a list
        subject => binary(),
        content_type => binary(),   % "text/plain", "text/html"
        body => binary(),
        attachments => [#{name => binary, content_type => binary, body => binary}],
        debug => boolean
    }.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Sends a mail message
-spec send(nkservice:id(), #nkmail_msg{} | mail_msg()) ->
    ok | {error, term()}.

send(Srv, #nkmail_msg{provider_id=ProvId}=Mail) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case SrvId:nkmail_get_provider(SrvId, ProvId) of
                {ok, Provider} ->
                    SrvId:nkmail_send(SrvId, Provider, Mail);
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end;

send(Srv, Msg) ->
    case nkmail_util:parse_msg(Msg) of
        {ok, #nkmail_msg{}=Mail} ->
            send(Srv, Mail);
        {error, Error} ->
            {error, Error}
    end.





