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

-module(nkmail_api_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([syntax/3]).
-export([msg_syntax/1, provider_syntax/0]).
-export([parse_msg_fun/2]).

%% ===================================================================
%% Syntax
%% ===================================================================

syntax('', send, Syntax) ->
    Msg = msg_syntax(true),
    maps:merge(Syntax, Msg);

syntax(_Sub, _Cmd, Syntax) ->
	Syntax.




%% ===================================================================
%% Message parsing
%% ===================================================================

%% @private
msg_syntax(Base64) ->
    #{
        provider => binary,
        from => fun ?MODULE:parse_msg_fun/2,
        to => fun ?MODULE:parse_msg_fun/2,
        subject => binary,
        content_type => fun ?MODULE:parse_msg_fun/2,
        body => binary,
        attachments =>
        {list,
            {syntax, #{
                name => binary,
                content_type => fun ?MODULE:parse_msg_fun/2,
                body => case Base64 of true -> base64; false -> binary end,
                '__mandatory' => [name, content_type, body]
            }}},
        debug => boolean,
        '__defaults' => #{
            subject => <<>>,
            content_type => <<"text/plain">>,
            body => <<>>,
            attachments => []
        },
        '__mandatory' => [provider, to]
    }.


%% @private
parse_msg_fun(from, <<>>) ->
    {error, {missing_field, <<"from">>}};

parse_msg_fun(from, Val) ->
    case parse_rfc822(Val) of
        {ok, [{Desc, Url}]} -> {ok, {Desc, Url}};
        error -> error
    end;

parse_msg_fun(from, Val) ->
    case parse_rfc822(Val) of
        {ok, [{Desc, Url}]} -> {ok, {Desc, Url}};
        error -> error
    end;

parse_msg_fun(to, Val) ->
    parse_rfc822(Val);

parse_msg_fun(content_type, Val) ->
    Val2 = to_bin(Val),
    case binary:split(Val2, <<"/">>) of
        [_, _] -> {ok, Val2};
        _ -> error
    end.


%% @private
parse_rfc822({Desc, Url}) when is_binary(Desc), is_binary(Url)->
    {ok, [{Desc, Url}]};

parse_rfc822([{Desc, Url}|_]=List) when is_binary(Desc), is_binary(Url)->
    {ok, List};

parse_rfc822(Key) ->
    case catch smtp_util:parse_rfc822_addresses(Key) of
        {ok, List} ->
            {ok, [
                {case Desc of undefined -> <<>>; _ -> to_bin(Desc) end, to_bin(Url)}
                || {Desc, Url} <- List
            ]};
        _ ->
            error
    end.



%% ===================================================================
%% Provider parsing
%% ===================================================================

%% @private
provider_syntax() ->
    #{
        id => binary,
        class => binary,
        from => fun ?MODULE:parse_msg_fun/2,
        config => map,
        '__mandatory' => [id, class, from]
    }.


%% @private
to_bin(T) -> nklib_util:to_binary(T).