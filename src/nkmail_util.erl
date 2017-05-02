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

%% @doc NkMAIL Utilities

-module(nkmail_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([parse_msg/1]).

-include("nkmail.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
parse_msg(Msg) ->
    Syntax = nkmail_api_syntax:msg_syntax(false),
    case nklib_syntax:parse(Msg, Syntax) of
        {ok, Parsed, _, []} ->
            #{
                provider := Provider,
                to := To,
                subject := Subject,
                content_type := CT,
                body := Body
            } = Parsed,
            Attachs = case maps:get(attachments, Parsed, []) of
                [] ->
                    [];
                List ->
                    lists:map(
                        fun(#{name:=Name, content_type:=ACT, body:=ABody}) ->
                            {Name, ACT, ABody}
                        end,
                        List)
            end,
            Mail = #nkmail_msg{
                provider_id = Provider,
                from = maps:get(from, Parsed, undefined),
                to = To,
                subject = Subject,
                content_type = CT,
                body = Body,
                attachments = Attachs,
                debug = maps:get(debug, Parsed, false)
            },
            {ok, Mail};
        {ok, _Parsed, _, [Key|_]} ->
            {error, {unknown_field, Key}};
        {error, Error} ->
            {error, Error}
    end.

