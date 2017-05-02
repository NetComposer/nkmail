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
-export([cmd/4]).

-include_lib("nkapi/include/nkapi.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Commands
%% ===================================================================


cmd('', send, #nkapi_req{tid=TId, data=Msg}, #{srv_id:=SrvId}=State) ->
    Self = self(),
    spawn_link(
        fun() ->
            Reply = nkmail:send(SrvId, Msg),
            nkapi_server:reply(Self, TId, Reply)
        end),
    {ack, State};

cmd(_Sub, _Cmd, _Data, State) ->
	{error, not_implemented, State}.



%% ===================================================================
%% Extract attachments
%% ===================================================================



