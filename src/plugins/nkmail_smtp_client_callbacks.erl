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

-module(nkmail_smtp_client_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0]).
-export([nkmail_parse_provider/2, nkmail_send/3]).


-include("nkmail.hrl").


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
    [nkmail].



%% ===================================================================
%% Mail callbacks
%% ===================================================================

%% @private
nkmail_parse_provider(Data, ParseOpts) ->
    nkmail_smtp_client:parse_provider(Data, ParseOpts).


%% @private
nkmail_send(_SrvId, #{class:=smtp}=Provider, Msg) ->
    nkmail_smtp_client:send(Msg, Provider);

nkmail_send(_SrvId, _Provider, _Msg) ->
    continue.




