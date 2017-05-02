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

-export([plugin_deps/0, plugin_syntax/0, plugin_start/2, plugin_stop/2]).
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
%%
%% These are used when NkMAIL is started as a NkSERVICE plugin
%% ===================================================================


plugin_deps() ->
    [].


plugin_syntax() ->
	#{
	}.


plugin_start(Config, #{id:=_SrvId}) ->
	{ok, Config}.


plugin_stop(Config, #{id:=_SrvId}) ->
    {ok, Config}.


%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
api_error(_) -> continue.



%% ===================================================================
%% Mail callbacks
%% ===================================================================


%% @doc Gets a mail provider
-spec nkmail_get_provider(nkservice:id(), nkmail:provider_id()) ->
    {ok, #nkmail_provider{}} | {error, term()}.

nkmail_get_provider(SrvId, Id) ->
    case nkmail_app:get_provider(nklib_util:to_binary(Id)) of
        {ok, Map} ->
            SrvId:nkmail_parse_provider(Map);
        not_found ->
            {error, not_found}
    end.


%% @doc Parses a mail provider
-spec nkmail_parse_provider(map()) ->
    {ok, #nkmail_provider{}} | {error, term()}.

nkmail_parse_provider(_Provider) ->
    {error, invalid_provider}.


%% @doc Sends a mail message using a provider
-spec nkmail_send(nkservice:id(), #nkmail_provider{}, map()) ->
    ok | {error, term()}.

nkmail_send(_SrvId, _Provider, _Msg) ->
    error(invalid_mail_provider).




%% ===================================================================
%% API Server
%% ===================================================================

%% @doc
api_server_syntax(Syntax, #nkapi_req{class = <<"mail">>, subclass=Sub, cmd=Cmd}=Req, State) ->
    {nkmail_api_syntax:syntax(Sub, Cmd, Syntax), Req, State};

api_server_syntax(_Syntax, _Req, _State) ->
    continue.


%% @doc
api_server_cmd(#nkapi_req{class = <<"mail">>, subclass=Sub, cmd=Cmd, data=Data}, State) ->
    nkmail_api:cmd(Sub, Cmd, Data, State);

api_server_cmd(_Req, _State) ->
    continue.

