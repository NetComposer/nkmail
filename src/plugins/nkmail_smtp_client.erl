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

-module(nkmail_smtp_client).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([send/2]).
-export([parse_provider/1, parse_msg/2]).
-export([parse_from/1, parse_to/1]).


-include("nkmail.hrl").

%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


%% @private
send(Msg, #nkmail_provider{config=Config}=Provider) ->
    case parse_msg(Msg, Provider) of
        {ok, Mail} ->
            Opts = make_send_opts(maps:to_list(Config), []),
            lager:error("DO SEND\n~p\n ~p", [Mail, Opts]),
            case gen_smtp_client:send_blocking(Mail, Opts) of
                <<"2.0.0 OK", _/binary>> ->
                    ok;
                Other ->
                    {error, {smtp_error, Other}}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @private
parse_provider(Data) ->
    Syntax = #{
        id => binary,
        class => atom,
        config => #{
            relay => binary,
            port => integer,
            username => binary,
            password => binary,
            force_tls => boolean,
            force_ath => boolean,
            from => binary
        },
        '__mandatory' => [id, class]
    },
    case nklib_syntax:parse(Data, #{class=>atom}) of
        {ok, #{class:=smtp}, _, _} ->
            case nklib_syntax:parse(Data, Syntax) of
                {ok, #{id:=Id, class:=smtp} = Parsed, _, _} ->
                    Provider = #nkmail_provider{
                        id = Id,
                        class = smtp,
                        config = maps:get(config, Parsed, #{})
                    },
                    {ok, Provider};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end.

parse_msg(Msg, #nkmail_provider{config=Config}) ->
    Syntax = #{
        from => fun ?MODULE:parse_from/1,
        to => fun ?MODULE:parse_to/1,
        subject => binary,
        content_type => binary,
        body => binary,
        attachments =>
            {list,
                {syntax, #{
                    name => binary,
                    content_type => binary,
                    body => binary
                    % '__mandatory' => [name, content_type, body]
                }}},
        '__defaults' => #{
            from => maps:get(username, Config, <<>>),
            subject => <<>>,
            content_type => <<"text/plain">>,
            body => <<>>,
            attachments => []
        },
        '__mandatory' => [to]
    },
    case nklib_syntax:parse(Msg, Syntax) of
        {ok, #{attachments:=Attachs}=Parsed, _, []} ->
            case Attachs of
                [] ->
                    parse_msg_simple(Parsed);
                _ ->
                    parse_msg_attachments(Parsed)
            end;
        {ok, _Parsed, _, [Key|_]} ->
            {error, {unknown_field, Key}};
        {error, Error} ->
            {error, Error}
    end.


parse_msg_simple(Msg) ->
    #{
        from := {FromDesc, FromUrl},
        to := To,
        subject := Subject,
        content_type := CT,
        body := Body
    } = Msg,
    ToUrls = [ToUrl || {_, ToUrl} <- To],
    case CT of
        <<"text/plain">> ->
            Mail = list_to_binary([
                "Subject: ", Subject, "\r\n",
                "From: ", FromDesc, " <", FromUrl, ">\r\n",
                [["To: ",   ToDesc, " <", ToUrl, ">\r\n"] || {ToDesc, ToUrl} <- To],
                "\r\n",
                Body
            ]),
            {ok, {FromUrl, ToUrls, Mail}};
        <<"text/html">> ->
            Mime = {
                <<"text">>,
                <<"html">>,
                    [
                        {<<"From">>, <<FromDesc/binary, " <", FromUrl/binary, ">">>},
                        {<<"Subject">>, Subject}
                    ] ++
                    [{<<"To">>, <<ToDesc/binary, " <", ToUrl/binary, ">">>} || {ToDesc, ToUrl} <- To],
                [
                ],
                Body
            },
            Mail = mimemail:encode(Mime),
            {ok, {FromUrl, ToUrls, Mail}};
        _ ->
            {error, invalid_content_type}
    end.


parse_msg_attachments(Msg) ->
    #{
        from := {FromDesc, FromUrl},
        to := To,
        subject := Subject,
        content_type := CT,
        body := Body,
        attachments := Attachments
    } = Msg,
    ToUrls = [ToUrl || {_, ToUrl} <- To],
    {ok, Attachs} = get_attachments(Attachments, []),
    [CT1, CT2] = binary:split(CT, <<"/">>),
    MimeBody = {
        CT1,
        CT2,
        [
            {<<"Content-Type">>, <<"text/plain;charset=utf-8">>},
            {<<"Content-Transfer-Encoding">>, <<"quoted-printable">>},
            {<<"Content-Disposition">>, <<"inline">>}
        ],
        [],
        Body},
    MimeMail = {
        <<"multipart">>,
        <<"mixed">>,
        [
            {<<"From">>, <<FromDesc/binary, " <", FromUrl/binary, ">">>},
            {<<"Subject">>, Subject}
        ] ++
        [{<<"To">>, <<ToDesc/binary, " <", ToUrl/binary, ">">>} || {ToDesc, ToUrl} <- To],
        [],
        [MimeBody | Attachs]
    },
    Mail = mimemail:encode(MimeMail),
    {ok, {FromUrl, ToUrls, Mail}}.


%% @private
get_attachments([], Acc) ->
    {ok, Acc};

get_attachments([#{name:=Name, content_type:=CT, body:=Body}|Rest], Acc) ->
    [Ct1, Ct2] = binary:split(CT, <<"/">>),
    M = {Ct1, Ct2,
        [
            {<<"Content-Transfer-Encoding">>, <<"base64">>}
        ],
        [
            {<<"disposition">>, <<"attachment">>},
            {<<"disposition-params">>, [{<<"filename">>, Name}]}
        ],
        Body},
    get_attachments(Rest, [M|Acc]).



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
%% Other options hostname, retry
make_send_opts([], Acc) ->
    Acc;
make_send_opts([{Key, Val}|Rest], Acc) when Key==relay; Key==username; Key==password ->
    make_send_opts(Rest, [{Key, Val}|Acc]);

make_send_opts([{force_tls, true}|Rest], Acc) ->
    make_send_opts(Rest, [{tls, always}|Acc]);

make_send_opts([{force_auth, true}|Rest], Acc) ->
    make_send_opts(Rest, [{auth, always}|Acc]);

make_send_opts([_|Rest], Acc) ->
    make_send_opts(Rest, Acc).



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
parse_from(<<>>) ->
    {error, {missing_field, <<"from">>}};

parse_from(Key) ->
    case parse_rfc822(Key) of
        {ok, [{Desc, Url}]} -> {ok, {Desc, Url}};
        error -> error
    end.


%% @private
parse_to(Key) ->
    parse_rfc822(Key).


%% @private
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


to_bin(T) -> nklib_util:to_binary(T).
