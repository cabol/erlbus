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
-export([subscribers/1, channels/0]).
-export([dispatch/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec sub(ebus:channel(), ebus:handler() | [ebus:handler()]) -> ok.
sub(Channel, Handlers) when is_list(Handlers) ->
  lists:foreach(fun(Handler) -> sub(Channel, Handler) end, Handlers);
sub(Channel, Handler) ->
  Key = {p, l, {?MODULE, Channel}},
  gproc_lib:insert_reg(Key, gproc:default(Key), Handler, l),
  ok.

-spec unsub(ebus:channel(), ebus:handler() | [ebus:handler()]) -> ok.
unsub(Channel, Handlers) when is_list(Handlers) ->
  lists:foreach(fun(Handler) -> unsub(Channel, Handler) end, Handlers);
unsub(Channel, Handler) ->
  Key = {p, l, {?MODULE, Channel}},
  gproc_lib:remove_reg(Key, Handler, unreg),
  ok.

-spec pub(ebus:channel(), ebus:payload()) -> ok.
pub(Channel, Message) ->
  gproc:send({p, l, {?MODULE, Channel}}, {ebus, {Channel, Message}}),
  ok.

-spec subscribers(ebus:channel()) -> [ebus:handler()].
subscribers(Channel) ->
  Key = {p, l, {?MODULE, Channel}},
  gproc:lookup_pids(Key).

-spec channels() -> [ebus:channel()].
channels() ->
  Pattern = {{{p, l, {?MODULE, '$1'}}, '_'}, '_', '_'},
  L = [N || [N] <- ets:match(gproc, Pattern)],
  ebus_util:rem_dups_from_list(L).

-spec dispatch(ebus:channel(), ebus:payload(), ebus:handler()) -> ok.
dispatch(Channel, Message, Handler) ->
  Handler ! {ebus, {Channel, Message}},
  ok.
