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

-module(nkmail_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([error/1]).
-export([nkmail_parse_provider/2, nkmail_send/2]).

-include("nkmail.hrl").



%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error({smtp_error, Error})          -> {"SMTP error: ~p", [Error]};
error(provider_class_not_found )      -> "Invalid provider class";
error(_)                            -> continue.



%% ===================================================================
%% Mail callbacks
%% ===================================================================


%% @doc Parses a mail provider
-spec nkmail_parse_provider(nkmail:provider(), nklib_syntax:syntax()) ->
    {ok, nkmail:provider(), [binary()]} | {error, term()} | continue().

nkmail_parse_provider(_Provider, _ParserOpts) ->
    {error, provider_class_not_found}.


%% @doc Sends a mail message using a provider
-spec nkmail_send(nkservice:id(), map()) ->
    ok | {error, term()} | continue().

nkmail_send(_SrvId, _Msg) ->
    erlang:error(provider_class_not_found).

