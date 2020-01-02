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

%% @doc NkMAIL

-module(nkmail).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([send/2]).
-export_type([msg/0]).

-include("nkmail.hrl").
-include_lib("nkserver/include/nkserver.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% Options dependant of storage class
%% ----------------------------------
%%
%% - id
%% - name: mandatory for filesystem
%% - password
%% - contentType

-type meta() :: map().


-type msg() ::
    #{
        from => binary(),           % Optional, can be "a@a.com" or "Name <a@a.com>"
        to => binary(),             % Can be a list
        subject => binary(),
        content_type => binary(),   % "text/plain", "text/html"
        body => binary(),
        attachments => [
            #{name => binary(), content_type => binary(), body => binary()}
        ],
        debug => boolean()
    }.



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Sends a file to the backend
-spec send(nkserver:id(), msg()) ->
    {ok, meta()} | {error, term()}.

send(SrvId, Msg) ->
    case nklib_syntax:parse(Msg, msg_syntax()) of
        {ok, Msg2, _} ->
            ?CALL_SRV(SrvId, nkmail_send, [SrvId, Msg2]);
        {error, Error} ->
            {error, Error}
    end.



%% ===================================================================
%% Internal
%% ===================================================================




%% @doc
msg_syntax() ->
    #{
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
        '__mandatory' => [to]
    }.