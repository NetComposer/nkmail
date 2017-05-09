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
-export([api_error/1]).
-export([nkmail_get_provider/2, nkmail_parse_provider/1, nkmail_send/3]).

-export([api_server_cmd/2, api_server_syntax/3]).

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
        fun(Data) ->
            case nkmail:parse_provider(SrvId, Data) of
                {ok, #{id:=Id}=Provider} ->
                    lager:info("NkMAIL: loading provider ~s", [Id]),
                    nkmail_app:put_provider(Provider);
                {error, Error} ->
                    lager:warning("NkMAIL: could not load provider ~p: ~p", [Data, Error])
            end
        end,
        nkmail_app:get_providers()),
    {ok, State}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
api_error({smtp_error, Error})          -> {"SMTP error: ~p", [Error]};
api_error(invalid_provider_class )      -> "Invalid provider class";
api_error({provider_not_found, Id})     -> {"Provider not found: ~p", [Id]};
api_error(_)                            -> continue.



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
-spec nkmail_parse_provider(map()) ->
    {ok, nkmail:provider()} | {error, term()}.

nkmail_parse_provider(_Provider) ->
    {error, invalid_provider_class}.


%% @doc Sends a mail message using a provider
-spec nkmail_send(nkservice:id(), nkmail:provier(), map()) ->
    ok | {error, term()}.

nkmail_send(_SrvId, _Provider, _Msg) ->
    error(invalid_mail_provider).


%% ===================================================================
%% API Server
%% ===================================================================

%% @doc
api_server_syntax(Syntax, #nkapi_req{class=nkmail, subclass=Sub, cmd=Cmd}=Req, State) ->
    {nkmail_api_syntax:syntax(Sub, Cmd, Syntax), Req, State};

api_server_syntax(_Syntax, _Req, _State) ->
    continue.


%% @doc
api_server_cmd(#nkapi_req{class=nkmail, subclass=Sub, cmd=Cmd}=Req, State) ->
    nkmail_api:cmd(Sub, Cmd, Req, State);

api_server_cmd(_Req, _State) ->
    continue.

