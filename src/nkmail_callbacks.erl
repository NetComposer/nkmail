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

-export([msg/1]).
-export([nkmail_send/4]).

-include("nkmail.hrl").



%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
msg({smtp_error, Error})          -> {"SMTP error: ~p", [Error]};
msg(backend_class_not_found )      -> "Invalid backend class";
msg(_)                            -> continue.



%% ===================================================================
%% Mail callbacks
%% ===================================================================


%% @doc Sends a mail message using a provider
-spec nkmail_send(nkservice:id(), nkpackage:id(), Class::binary(), nkmail:msg()) ->
    ok | {error, term()} | continue().

nkmail_send(_SrvId, _PackageId, _Class, _Msg) ->
    {error, backend_class_not_found}.

