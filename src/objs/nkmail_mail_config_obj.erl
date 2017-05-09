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

%% @doc Config Object

-module(nkmail_mail_config_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/4, load_providers/2]).
-export([object_get_info/0, object_mapping/0, object_syntax/1,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).

-include("nkmail.hrl").

-define(LLOG(Type, Txt, Args),
    lager:Type("NkMAIL Config "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% API
%% ===================================================================



%% @doc
%% Data must follow object's syntax
-spec create(nkservice:id(), nkdomain:id(), nkdomain:name(), map()) ->
    {ok, nkdomain:obj_id(), nkdomain:path(), pid()} | {error, term()}.

create(Srv, Parent, Name, Provider) ->
    case nkmail:parse_provider(Srv, Provider) of
        {ok, _} ->
            Opts = #{
                obj_name => Name,
                type_obj => Provider,
                subtype => <<"config">>
            },
            nkdomain_obj_lib:make_and_create(Srv, Parent, ?DOMAIN_MAIL_CONFIG, Opts);
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec load_providers(nkservice:id(), nkdomain:id()) ->
    [ProvId::nkdomain:obj_id()].

load_providers(Srv, Parent) ->
    ProvIds = nkmail_app:get_provider_ids(),
    lists:foreach(
        fun(Id) ->
            Provider = nkmail_app:get_provider(Id),
            case create(Srv, Parent, Id, Provider) of
                {ok, ObjId, _Path, _Pid} ->
                    ?LLOG(info, "Loaded provider ~s (~s)", [Id, ObjId]);
                {error, Error} ->
                    ?LLOG(warning, "Could not load provider ~s: ~p", [Id, Error])
            end
        end,
        ProvIds).






%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_MAIL_CONFIG
    }.


%% @private
object_mapping() ->
    nkdomain_config_obj:object_mapping().


%% @private
object_syntax(_) ->
    any.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkdomain_config_obj:object_api_syntax(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Req, State) ->
    nkdomain_config_obj:object_api_cmd(Sub, Cmd, Req, State).





