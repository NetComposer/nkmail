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
-export([parse_msg_fun/2]).

-compile(export_all).

-include("nkmail.hrl").

%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================


%% @doc
send(Msg, #nkmail_provider{config=Config}=Provider) ->
    {Mail, Debug} = parse_msg(Msg, Provider),
    Opts = make_send_opts(maps:to_list(Config), []),
    case gen_smtp_client:send_blocking(Mail, Opts) of
        <<"2.0.0 OK", _/binary>> = Reply ->
            case Debug of
                true ->
                    lager:debug("Message sent OK: ~s\n~s", [Reply, element(3, Mail)]);
                false ->
                    ok
            end,
            ok;
        Other ->
            {error, {smtp_error, Other}}
    end.


%% @doc
parse_provider(Data) ->
    case nklib_syntax:parse(Data, #{class=>atom}) of
        {ok, #{class:=smtp}, _, _} ->
            case nklib_syntax:parse(Data, provider_syntax()) of
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


%% @doc
parse_msg(Msg, #nkmail_provider{config=Config}) ->
    case nklib_syntax:parse(Msg, msg_syntax(Config)) of
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




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
parse_msg_simple(Msg) ->
    #{
        from := {FromDesc, FromUrl},
        to := To,
        subject := Subject,
        content_type := CT,
        body := Body
    } = Msg,
    ToUrls = [ToUrl || {_, ToUrl} <- To],
    Debug = maps:get(debug, Msg, false),
    case CT of
        <<"text/plain">> ->
            Mail = list_to_binary([
                "Subject: ", Subject, "\r\n",
                "From: ", FromDesc, " <", FromUrl, ">\r\n",
                [["To: ",   ToDesc, " <", ToUrl, ">\r\n"] || {ToDesc, ToUrl} <- To],
                "\r\n",
                Body
            ]),
            {{FromUrl, ToUrls, Mail}, Debug};
        _ ->
            [CT1, CT2] = binary:split(CT, <<"/">>),
            Mime = {
                CT1,
                CT2,
                    [
                        {<<"From">>, <<FromDesc/binary, " <", FromUrl/binary, ">">>},
                        {<<"Subject">>, Subject}
                    ] ++
                    [{<<"To">>, <<ToDesc/binary, " <", ToUrl/binary, ">">>} || {ToDesc, ToUrl} <- To],
                [
                    % If we select 7bit, mimemail does not change the message
                    % If we select quoted-printable, it uses some config
                    % If we select base64, it will encode the message as base64
                    % If empty it does whatever it thinks its 'best'
                    % For utf8 chars, all of them work
                    {<<"transfer-encoding">>, <<"base64">>}
                ],
                Body
            },
            Mail = mimemail:encode(Mime),
            {{FromUrl, ToUrls, Mail}, Debug}
    end.


%% @private
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
    Debug = maps:get(debug, Msg, false),
    Attachs = get_attachments(Attachments, []),
    [CT1, CT2] = binary:split(CT, <<"/">>),
    MimeBody = {CT1, CT2, [], [], Body},
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
    {{FromUrl, ToUrls, Mail}, Debug}.


%% @private
get_attachments([], Acc) ->
    Acc;

get_attachments([#{name:=Name, content_type:=CT, body:=Body}|Rest], Acc) ->
    [CT1, CT2] = binary:split(CT, <<"/">>),
    Mime = {
        CT1,
        CT2,
        [],
        [
            {<<"disposition">>, <<"attachment">>},
            {<<"disposition-params">>, [{<<"filename">>, Name}]}
        ],
        Body},
    get_attachments(Rest, [Mime|Acc]).


%% @private
msg_syntax(Config) ->
    #{
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
                    body => binary,
                    '__mandatory' => [<<"attachments.name">>, <<"attachments.content_type">>, <<"attachments.body">>]
                }}},
        '__defaults' => #{
            from => maps:get(username, Config, <<>>),
            subject => <<>>,
            content_type => <<"text/plain">>,
            body => <<>>,
            attachments => []
        },
        '__mandatory' => [to],
        debug => boolean
    }.


%% @private
provider_syntax() ->
    #{
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
    }.


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
parse_msg_fun(from, <<>>) ->
    {error, {missing_field, <<"from">>}};

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
