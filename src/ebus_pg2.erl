%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Andres Bolaños, Inc. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2015, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc EBus - Erlang PG2 provider.
%%%-------------------------------------------------------------------
-module(ebus_pg2).

-behaviour(ebus).

%% API
-export([sub/2, unsub/2, pub/2]).
-export([get_subscribers/1, get_channels/0]).
-export([dispatch/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec sub(ebus:channel(), ebus:handler() | [ebus:handler()]) -> ok.
sub(Channel, Handlers) when is_list(Handlers) ->
  lists:foreach(fun(Handler) -> sub(Channel, Handler) end, Handlers);
sub(Channel, Handler) ->
  case pg2:join(Channel, Handler) of
    ok ->
      ok;
    {error, {no_such_group, _}} ->
      ok = pg2:create(Channel),
      ok = pg2:join(Channel, Handler),
      ok
  end.

-spec unsub(ebus:channel(), ebus:handler() | [ebus:handler()]) -> ok.
unsub(Channel, Handlers) when is_list(Handlers) ->
  lists:foreach(fun(Handler) -> unsub(Channel, Handler) end, Handlers);
unsub(Channel, Handler) ->
  case pg2:leave(Channel, Handler) of
    ok ->
      ok;
    {error, {no_such_group, _}} ->
      ok = pg2:create(Channel),
      ok = pg2:leave(Channel, Handler),
      ok
  end.

-spec pub(ebus:channel(), ebus:payload()) -> ok.
pub(Channel, Msg) ->
  Pids = get_subscribers(Channel),
  lists:foreach(fun(Pid) -> Pid ! {ebus, {Channel, Msg}} end, Pids).

-spec get_subscribers(ebus:channel()) -> [ebus:handler()].
get_subscribers(Channel) ->
  case pg2:get_members(Channel) of
    {error, {no_such_group, _}} ->
      ok = pg2:create(Channel),
      pg2:get_members(Channel);
    Members ->
      Members
  end.

-spec get_channels() -> [ebus:channel()].
get_channels() ->
  pg2:which_groups().

-spec dispatch(ebus:channel(), ebus:payload(), ebus:handler()) -> ok.
dispatch(Channel, Msg, Handler) ->
  Handler ! {ebus, {Channel, Msg}},
  ok.
