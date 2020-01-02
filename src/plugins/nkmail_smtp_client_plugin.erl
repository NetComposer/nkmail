%% -------------------------------------------------------------------
%%
%% Copyright (c) 2020 Carlos Gonzalez Florido.  All Rights Reserved.
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

-module(nkmail_smtp_client_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_config/3]).

-include("nkmail.hrl").

%% ===================================================================
%% Plugin callbacks
%%
%% These are used when NkMAIL is started as a NkSERVICE plugin
%% ===================================================================


plugin_deps() ->
    [nkmail].

%% @doc
plugin_config(_SrvId, Config, #{class:=nkmail}) ->
    Syntax = #{
        smtp_relay => binary,
        smtp_port => integer,
        smtp_username => binary,
        smtp_password => binary,
        smtp_retries => {integer, 0, 10},
        smtp_hostname => binary,
        smtp_force_tls => boolean,
        smtp_force_auth => boolean
    },
    nkserver_util:parse_config(Config, Syntax).

