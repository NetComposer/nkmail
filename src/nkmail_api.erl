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

%% @doc NkMAIL external API

-module(nkmail_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([cmd/3]).

-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Commands
%% ===================================================================


cmd(<<"send">>, #nkreq{data=Msg, srv_id=SrvId}=Req, State) ->
    Self = self(),
    TId = nkapi_server:get_tid(Req),
    spawn_link(
        fun() ->
            Reply = nkmail:send(SrvId, Msg#{debug=>true}),
            nkapi_server:reply(Self, TId, Reply)
        end),
    {ack, State};

cmd(_Cmd, _Req, State) ->
	{error, not_implemented, State}.



%% ===================================================================
%% Extract attachments
%% ===================================================================



