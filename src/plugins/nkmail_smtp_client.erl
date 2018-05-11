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
-export([send/1, provider_syntax/0]).
-export_type([provider_config/0]).

-include("nkmail.hrl").

%% ===================================================================
%% Types
%% ===================================================================

-type provider_config() ::
    #{
        relay => binary(),
        port => integer(),
        username => binary(),
        password => binary(),
        retries => 0..10,
        hostname => binary(),
        force_tls => boolean(),
        force_auth => boolean()
    }.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc
send(#{provider:=Provider}=Msg) ->
    Mail = make_msg(Msg, Provider),
    Config = maps:to_list(maps:get(config, Provider, #{})),
    Opts = make_send_opts(Config, []),
    try gen_smtp_client:send_blocking(Mail, Opts) of
        Reply when is_binary(Reply) ->
            send_ok_reply(Reply, Msg, Mail);
        {error, _Type, Error} ->
            {error, {smtp_error, nklib_util:to_binary(Error)}};
        {error, Error} ->
            {error, {smtp_error, nklib_util:to_binary(Error)}}
%%        <<"2.0.0 OK", _/binary>> = Reply ->
%%            send_ok_reply(Reply, Msg, Mail);
%%        % Mailgun response (!)
%%        <<"Great success", _/binary>> = Reply ->
%%            send_ok_reply(Reply, Msg, Mail);
%%        Other ->
%%            {error, {smtp_error, nklib_util:to_binary(Other)}}
    catch
        throw:{permanent_failure, Text} ->
            {error, {smtp_error, Text}};
        throw:Error ->
            {error, {smtp_error, nklib_util:to_binary(Error)}}
    end.


%% @private
send_ok_reply(Reply, Msg, Mail) ->
    case maps:get(debug, Msg, false) of
        true ->
            lager:debug("Message sent OK: ~s\n~s", [Reply, element(3, Mail)]);
        false ->
            ok
    end,
    {ok, #{smtp_reply=>Reply}}.





%% @private
provider_syntax() ->
    #{
        class => atom,
        from => fun nkmail_util:parse_msg_fun/2,
        config => #{
            relay => binary,
            port => integer,
            username => binary,
            password => binary,
            retries => {integer, 0, 10},
            hostname => binary,
            force_tls => boolean,
            force_auth => boolean
        },
        '__defaults' => #{config => #{}},
        '__mandatory' => [class, from]
    }.


%% ===================================================================
%% Internal
%% ===================================================================

%% @doc
make_msg(Msg, Provider) ->
    case Msg of
        #{from:=_} ->
            do_make_msg(Msg);
        _ ->
            do_make_msg(Msg#{from=>maps:get(from, Provider)})
    end.


%% @private
do_make_msg(#{attachments:=[]}=Msg) ->
    #{
        from := From,
        to := To,
        subject := Subject,
        content_type := CT,
        body := Body
    }=Msg,
    case CT of
        <<"text/plain">> ->
            Mail = list_to_binary([
                "Subject: ", Subject, "\r\n",
                "From: ", From, "\r\n",
                [["To: ", ToSingle, "\r\n"] || ToSingle <- To],
                "\r\n",
                Body
            ]),
            {nkmail_util:get_url(From), nkmail_util:get_urls(To), Mail};
        _ ->
            [CT1, CT2] = binary:split(CT, <<"/">>),
            Mime = {
                CT1,
                CT2,
                    [
                        {<<"From">>, From},
                        {<<"Subject">>, Subject}
                    ] ++
                    [{<<"To">>, ToSingle} || ToSingle <- To],
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
            {nkmail_util:get_url(From), nkmail_util:get_urls(To), Mail}
    end;

do_make_msg(Msg) ->
    #{
        from := From,
        to := To,
        subject := Subject,
        content_type := CT,
        body := Body,
        attachments := Attachments
    }=Msg,
    Attachs = get_attachments(Attachments, []),
    [CT1, CT2] = binary:split(CT, <<"/">>),
    MimeBody = {CT1, CT2, [], [], Body},
    MimeMail = {
        <<"multipart">>,
        <<"mixed">>,
            [
                {<<"From">>, From},
                {<<"Subject">>, Subject}
            ] ++
            [{<<"To">>, ToSingle} || ToSingle <- To],
        [],
        [MimeBody | Attachs]
    },
    Mail = mimemail:encode(MimeMail),
    {nkmail_util:get_url(From), nkmail_util:get_urls(To), Mail}.


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
make_send_opts([], Acc) ->
    Acc;
make_send_opts([{Key, Val}|Rest], Acc) when Key==relay; Key==username; Key==password;
                                            Key==retries; Key==hostname ->
    make_send_opts(Rest, [{Key, Val}|Acc]);

make_send_opts([{force_tls, true}|Rest], Acc) ->
    make_send_opts(Rest, [{tls, always}|Acc]);

make_send_opts([{force_auth, true}|Rest], Acc) ->
    make_send_opts(Rest, [{auth, always}|Acc]);

make_send_opts([_|Rest], Acc) ->
    make_send_opts(Rest, Acc).


