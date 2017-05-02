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
-export([parse_provider/1]).

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
    {Mail, Debug} = make_msg(Msg, Provider),
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
make_msg(#nkmail_msg{from=MsgFrom}=Msg, #nkmail_provider{from=ProvFrom}) ->
    From = case MsgFrom of
        {_, _} -> MsgFrom;
        undefined -> ProvFrom
    end,
    do_make_msg(Msg#nkmail_msg{from=From}).




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
do_make_msg(#nkmail_msg{attachments=[]}=Msg) ->
    #nkmail_msg{
        from = {FromDesc, FromUrl},
        to = To,
        subject = Subject,
        content_type = CT,
        body = Body
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
            {FromUrl, ToUrls, Mail};
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
            {FromUrl, ToUrls, Mail}
    end;

do_make_msg(Msg) ->
    #nkmail_msg{
        from = {FromDesc, FromUrl},
        to = To,
        subject = Subject,
        content_type = CT,
        body = Body,
        attachments = Attachments
    } = Msg,
    ToUrls = [ToUrl || {_, ToUrl} <- To],
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
    {FromUrl, ToUrls, Mail}.


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
provider_syntax() ->
    #{
        id => binary,
        class => atom,
        from => binary,
        config => #{
            relay => binary,
            port => integer,
            username => binary,
            password => binary,
            force_tls => boolean,
            force_ath => booleanonk
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

