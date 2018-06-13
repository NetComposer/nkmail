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

-module(nkmail_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_api/1, plugin_config/3]).

-include("nkmail.hrl").


%% ===================================================================
%% Plugin callbacks
%% ===================================================================


%% @doc
plugin_deps() ->
    [].

%% @doc
plugin_api(?PKG_MAIL) ->
    #{
        luerl => #{
            send => {nkmail, luerl_send}
        }
    };

plugin_api(_Class) ->
    #{}.


%% @doc
plugin_config(?PKG_MAIL, #{id:=Id, config:=Config}=Spec, _Service) ->
    Syntax = #{
        backendClass => binary,
        from => binary,
        debug => boolean,
        '__mandatory' => [backendClass]
    },
    case nklib_syntax:parse(Config, Syntax, #{allow_unknown=>true}) of
        {ok, Parsed, _} ->
            DebugMap1 = nkservice_config_util:get_debug_map(Spec),
            Debug = maps:get(debug, Parsed, false),
            DebugMap2 = nkservice_config_util:set_debug_key(nkmail, Id, debug, Debug, DebugMap1),
            Spec2 = nkservice_config_util:set_debug_map(DebugMap2, Spec),
            CacheMap1 = nkservice_config_util:get_cache_map(Spec2),
            case nkservice_config_util:get_cache_key(nkmail, Id, backend_class, CacheMap1) of
                undefined ->
                    {error, unknown_backend_class};
                _ ->
                    From = maps:get(from, Parsed, <<>>),
                    CacheMap2 = nkservice_config_util:set_cache_key(nkmail, Id, from, From, CacheMap1),
                    Spec3 = nkservice_config_util:set_cache_map(CacheMap2, Spec2),
                    {ok, Spec3#{config:=Parsed}}
            end;
        {error, Error} ->
            {error, Error}
    end;

plugin_config(_Class, _Package, _Service) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================
