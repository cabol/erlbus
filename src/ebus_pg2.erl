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
-export([get_subscribers/1, get_topics/0]).
-export([dispatch/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec sub(ebus:topic(), ebus:handler()) -> ok.
sub(Topic, Handler) ->
  case pg2:join(Topic, Handler) of
    ok ->
      ok;
    {error, {no_such_group, _}} ->
      ok = pg2:create(Topic),
      ok = pg2:join(Topic, Handler),
      ok
  end.

-spec unsub(ebus:topic(), ebus:handler()) -> ok.
unsub(Topic, Handler) ->
  case pg2:leave(Topic, Handler) of
    ok ->
      ok;
    {error, {no_such_group, _}} ->
      ok = pg2:create(Topic),
      ok = pg2:leave(Topic, Handler),
      ok
  end.

-spec pub(ebus:topic(), ebus:payload()) -> ok.
pub(Topic, Msg) ->
  Pids = get_subscribers(Topic),
  lists:foreach(fun(Pid) -> Pid ! {ebus, {Topic, Msg}} end, Pids).

-spec get_subscribers(ebus:topic()) -> [ebus:handler()].
get_subscribers(Topic) ->
  case pg2:get_members(Topic) of
    {error, {no_such_group, _}} ->
      ok = pg2:create(Topic),
      pg2:get_members(Topic);
    Members ->
      Members
  end.

-spec get_topics() -> [ebus:topic()].
get_topics() ->
  pg2:which_groups().

-spec dispatch(ebus:topic(), ebus:payload(), ebus:handler()) -> ok.
dispatch(Topic, Msg, Handler) ->
  Handler ! {ebus, {Topic, Msg}},
  ok.
