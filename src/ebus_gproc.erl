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
%%% @doc EBus - Gproc provider.
%%%-------------------------------------------------------------------
-module(ebus_gproc).

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
  Key = {p, l, {?MODULE, Topic}},
  gproc_lib:insert_reg(Key, gproc:default(Key), Handler, l),
  ok.

-spec unsub(ebus:topic(), ebus:handler()) -> ok.
unsub(Topic, Handler) ->
  Key = {p, l, {?MODULE, Topic}},
  gproc_lib:remove_reg(Key, Handler, unreg),
  ok.

-spec pub(ebus:topic(), ebus:message()) -> ok.
pub(Topic, Message) ->
  gproc:send({p, l, {?MODULE, Topic}}, {ebus, {Topic, Message}}),
  ok.

-spec get_subscribers(ebus:topic()) -> [ebus:handler()].
get_subscribers(Topic) ->
  Key = {p, l, {?MODULE, Topic}},
  gproc:lookup_pids(Key).

-spec get_topics() -> [ebus:topic()].
get_topics() ->
  Pattern = {{{p, l, {?MODULE, '$1'}}, '_'}, '_', '_'},
  L = [N || [N] <- ets:match(gproc, Pattern)],
  ebus_util:rem_dups_from_list(L).

-spec dispatch(ebus:topic(), ebus:message(), ebus:handler()) -> ok.
dispatch(Topic, Message, Handler) ->
  Handler ! {ebus, {Topic, Message}},
  ok.
