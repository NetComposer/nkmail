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

-export([parse_msg/1, unparse_msg/1, msg_syntax/0, get_url/1, get_urls/1]).
-export([parse_msg_fun/2]).

-include("nkmail.hrl").


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
parse_msg(Msg) ->
    Syntax = msg_syntax(),
    case nklib_syntax:parse(Msg, Syntax) of
        {ok, Parsed, []} ->
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


%% @doc
msg_syntax() ->
    #{
        provider => binary,
        from => fun ?MODULE:parse_msg_fun/2,
        to => fun ?MODULE:parse_msg_fun/2,
        subject => binary,
        content_type => fun ?MODULE:parse_msg_fun/2,
        body => binary,
        attachments =>
        {list,
            #{
                name => binary,
                content_type => fun ?MODULE:parse_msg_fun/2,
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


%% @doc
unparse_msg(Msg) ->
    #nkmail_msg{
        provider_id = Provider,
        from = From,
        to = To,
        subject = Subject,
        content_type = CT,
        body = Body,
        attachments = Attachs,
        debug = Debug
    } = Msg,
    Attachments = lists:map(
        fun({N, C, B}) -> #{name=>N, content_type=>C, bod=>B} end,
        Attachs),
    #{
        provider_id => Provider,
        from => From,
        to => To,
        subject => Subject,
        content_type => CT,
        body => Body,
        attachments => Attachments,
        debug => Debug
    }.


%% @private
parse_msg_fun(from, Val) ->
    case parse_rfc822(Val) of
        {ok, [From]} -> {ok, From};
        _ -> error
    end;

parse_msg_fun(to, [First|_]=Val) when is_binary(First) ->
    parse_msg_fun(to, nklib_util:bjoin(Val));

parse_msg_fun(to, Val) ->
    parse_rfc822(Val);

parse_msg_fun(content_type, Val) ->
    Val2 = to_bin(Val),
    case binary:split(Val2, <<"/">>) of
        [_, _] -> {ok, Val2};
        _ -> error
    end.


%% @private
parse_rfc822(Val) ->
    case catch smtp_util:parse_rfc822_addresses(Val) of
        {ok, List} ->
            List2 = lists:map(
                fun
                    ({undefined, Url}) -> list_to_binary([$<, Url, $>]);
                    ({Name, Url}) -> list_to_binary([Name, " ", $<, Url, $>])
                end,
                List),
            {ok, List2};
        _ ->
            error
    end.



%% @doc
get_url(Bin) ->
    [_, Url1] = binary:split(Bin, <<"<">>),
    <<Url1:(byte_size(Url1)-1)/binary>>.


%% @doc
get_urls(Bins) ->
    get_urls(Bins, []).


%% @private
get_urls([], Acc) ->
    lists:reverse(Acc);

get_urls([Bin|Rest], Acc) ->
    get_urls(Rest, [get_url(Bin)|Acc]).


%% @private
to_bin(T) -> nklib_util:to_binary(T).


