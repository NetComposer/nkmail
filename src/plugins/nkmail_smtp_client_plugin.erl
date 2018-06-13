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
plugin_config(?PKG_MAIL, #{id:=Id, config:=Config}=Spec, _Service) ->
    case nklib_syntax:parse(Config, #{backendClass=>binary}) of
        {ok, #{backendClass:=<<"smtp">>}, _} ->
            Syntax = #{
                relay => binary,
                port => integer,
                username => binary,
                password => binary,
                retries => {integer, 0, 10},
                hostname => binary,
                force_tls => boolean,
                force_auth => boolean
            },
            case nklib_syntax:parse(Config, Syntax, #{allow_unknown=>true}) of
                {ok, Parsed, _} ->
                    CacheMap1 = nkservice_config_util:get_cache_map(Spec),
                    CacheMap2 = nkservice_config_util:set_cache_key(nkmail, Id, backend_class, <<"smtp">>, CacheMap1),
                    CacheMap3 = nkservice_config_util:set_cache_key(nkmail_smtp, Id, config, Parsed, CacheMap2),
                    Spec2 = nkservice_config_util:set_cache_map(CacheMap3, Spec),
                    {ok, Spec2#{config := Parsed}};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end;

plugin_config(_Class, _Package, _Service) ->
    continue.
