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

-module(nkmail_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([api/4, conn_syntax/0, msg_syntax/0, alert_syntax/0]).
-export([parse_alert/1]).


%% ===================================================================
%% Syntax
%% ===================================================================



api(_Cmd, Syntax, Defaults, Mandatory) ->
	{Syntax, Defaults, Mandatory}.






conn_syntax() ->
	#{
		conn_id => binary,
		sandbox => boolean,
        cert_file => binary,
        key_file => binary,
      	cert_password => binary,
		timeout => {integer, 1, none},
        feedback_timeout => {integer, 1, none},
		expires_conn =>{integer, 1, none}
	}.


msg_syntax() ->
	#{
		conn_id => binary,
		expiry => {integer, 1, none},
		device_token => binary,
		content_available => boolean,
        alert => fun ?MODULE:parse_alert/1,
        badge => integer,
        category => binary,
        sound => binary,
        priority => integer
    }.


alert_syntax() ->
	#{
		body => binary,
		action => binary,
		key => binary,
		args => {list, binary},
		image => binary
	}.


parse_alert(Key) when is_integer(Key); is_atom(Key); is_list(Key); is_binary(Key) ->
	{ok, nklib_util:to_binary(Key)};

parse_alert(Map) when is_map(Map) ->
	case nklib_config:parse_config(Map, alert_syntax(), #{return=>map}) of
		{ok, Data, _} -> {ok, Data};
		{error, Error} -> {error, Error}
	end;

parse_alert(_) ->
	error.



