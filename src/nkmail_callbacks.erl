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

%% @doc NkMAIL callbacks

-module(nkmail_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, service_init/2]).
-export([error/1]).
-export([nkmail_get_provider/2, nkmail_parse_provider/2, nkmail_send/3]).
-export([service_api_cmd/2, service_api_syntax/2]).

-include("nkmail.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkapi/include/nkapi.hrl").



%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Plugin callbacks
%% ===================================================================


plugin_deps() ->
    [].


service_init(_Service, #{id:=SrvId}=State) ->
    % Loads app providers
    lists:foreach(
        fun
            (#{id:=Id}=Data) ->
                case nkmail:parse_provider(SrvId, Data) of
                    {ok, Provider, _} ->
                        lager:info("NkMAIL: loading provider ~s", [Id]),
                        nkmail_app:put_provider(nklib_util:to_binary(Id), Provider);
                    {error, Error} ->
                        lager:warning("NkMAIL: could not load provider ~p: ~p", [Data, Error])
                end;
            (Data) ->
                lager:warning("NkMAIL: invalid provider: ~p", [Data])
        end,
        nkmail_app:get(providers, [])),
    {ok, State}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error({smtp_error, Error})          -> {"SMTP error: ~p", [Error]};
error(invalid_provider_class )      -> "Invalid provider class";
error({provider_not_found, Id})     -> {"Provider not found: ~p", [Id]};
error(_)                            -> continue.



%% ===================================================================
%% Mail callbacks
%% ===================================================================


%% @doc Gets a mail provider
-spec nkmail_get_provider(nkservice:id(), nkmail:provider_id()) ->
    {ok, nkmail:provider()} | {error, term()}.

nkmail_get_provider(_SrvId, Id) ->
    case nkmail_app:get_provider(Id) of
        not_found ->
            {error, {provider_not_found, Id}};
        Provider ->
            {ok, Provider}
    end.


%% @doc Parses a mail provider
-spec nkmail_parse_provider(map(), nklib_syntax:parse_opts()) ->
    {ok, nkmail:provider(), [binary()]} | {error, term()}.

nkmail_parse_provider(_Provider, _Opts) ->
    {error, invalid_provider_class}.


%% @doc Sends a mail message using a provider
-spec nkmail_send(nkservice:id(), nkmail:provier(), map()) ->
    ok | {error, term()}.

nkmail_send(_SrvId, _Provider, _Msg) ->
    erlang:error(invalid_mail_provider).


%% ===================================================================
%% API Server
%% ===================================================================

%% @doc
service_api_syntax(Syntax, #nkreq{cmd = <<"nkmail/send">>}=Req) ->
    Syntax2 = Syntax#{
        provider => binary,
        from => binary,
        to => binary,                   % Allows several (with comma)
        subject => binary,
        content_type => binary,
        body => binary,
        attachments =>
        {list,
         #{
             name => binary,
             content_type => binary,
             body => base64,
             '__mandatory' => [name, content_type, body]
         }},
        '__mandatory' => [provider, to]
    },
    {Syntax2, Req};

service_api_syntax(_Syntax, _Req) ->
    continue.


%% @doc
service_api_cmd(#nkreq{cmd = <<"nkmail/send">>, data=Msg, srv_id=SrvId}=Req, State) ->
    Self = self(),
    TId = nkapi_server:get_tid(Req),
    spawn_link(
        fun() ->
            Reply = nkmail:send(SrvId, Msg#{debug=>true}),
            nkapi_server:reply(Self, TId, Reply)
        end),
    {ack, State};

service_api_cmd(_Req, _State) ->
    continue.

