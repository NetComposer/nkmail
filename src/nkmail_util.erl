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

%% @doc NkMAIL Utilities

-module(nkmail_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_url/1, get_urls/1]).
-export([parse_msg_fun/2]).

-include("nkmail.hrl").


%% ===================================================================
%% Public
%% ===================================================================

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


