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

%% @doc NkMAIL

-module(nkmail).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([send/2, parse_provider/2]).
-export_type([provider/0, msg/0]).

-include("nkmail.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type provider_class() :: atom().
-type provider_config() :: map().


-type provider() ::
    #{
        class => provider_class(),      % A plugin must implement it
        from => binary(),               % Default from
        config => provider_config()     % Specific for each plugin
    }.


-type msg() ::
    #{
        from => binary(),           % Optional, can be "a@a.com" or "Name <a@a.com>"
        to => binary(),             % Can be a list
        subject => binary(),
        content_type => binary(),   % "text/plain", "text/html"
        body => binary(),
        attachments => [#{name => binary, content_type => binary, body => binary}],
        debug => boolean,
        provider => provider()      % Mandatory
    }.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Sends a mail message
-spec send(nkservice:id(), msg()) ->
    {ok, Meta::map()} | {error, term()}.

send(SrvId, Msg) ->
    case nklib_syntax:parse(Msg, msg_syntax()) of
        {ok, #{provider:=Provider}=ParsedMsg, []} ->
            case parse_provider(SrvId, Provider) of
                {ok, ParsedProvider, _} ->
                    Msg2 = ParsedMsg#{provider=>ParsedProvider},
                    ?CALL_SRV(SrvId, nkmail_send, [SrvId, Msg2]);
                {error, Error} ->
                    {error, Error}
            end;
        {ok, _Parsed, _, [Key|_]} ->
            {error, {unknown_field, Key}};
        {error, Error} ->
            {error, Error}
    end.



%% @doc Parses a provider
-spec parse_provider(nkservice:id(), provider()) ->
    {ok, provider()} | {error, term()}.

parse_provider(SrvId, Map) ->
    case ?CALL_SRV(SrvId, nkmail_parse_provider, [Map, #{}]) of
        {ok, Provider, UnknownFields} ->
            {ok, Provider, UnknownFields};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
msg_syntax() ->
    #{
        provider => map,
        from => fun nkmail_util:parse_msg_fun/2,
        to => fun nkmail_util:parse_msg_fun/2,
        subject => binary,
        content_type => fun nkmail_util:parse_msg_fun/2,
        body => binary,
        attachments => {list,
             #{
                 name => binary,
                 content_type => fun nkmail_util:parse_msg_fun/2,
                 body => binary,
                 '__mandatory' => [name, content_type, body]
             }},
        debug => boolean,
        '__defaults' => #{
            subject => <<>>,
            content_type => <<"text/plain">>,
            body => <<>>,
            attachments => []
        },
        '__mandatory' => [provider, to]
    }.