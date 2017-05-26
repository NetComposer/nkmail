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
-export([syntax/2]).

%% ===================================================================
%% Syntax
%% ===================================================================

syntax(<<"send">>, Syntax) ->
    Syntax#{
        provider => binary,
        from => binary,
        to => binary,                   % Allows several (with comma)
        subject => binary,
        content_type => binary,
        body => binary,
        attachments =>
            {list,
                #{
                    name => binary,
                    content_type => binary,
                    body => base64,
                    '__mandatory' => [name, content_type, body]
                }},
        '__mandatory' => [provider, to]
    };

syntax(_Cmd, Syntax) ->
	Syntax.



