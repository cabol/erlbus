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
%%% @doc EBus App.
%%%-------------------------------------------------------------------
-module(ebus_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, start_phase/3, stop/0, stop/1]).

-define(DEFAULT_NAME, ebus).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start() -> {ok, _} | {error, term()}.
start() ->
  application:ensure_all_started(ebus).

-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
  ebus_global_sup:start_link(),
  ebus:new(?DEFAULT_NAME).

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(start_providers, _StartType, []) ->
  application:start(gproc),
  start_riak_core(),
  ok.

-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(ebus).

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
start_riak_core() ->
  case application:get_env(ebus_dist) of
    {ok, _} ->
      {ok, _} = application:ensure_all_started(riak_core),
      ok = riak_core:register(ebus, [{vnode_module, ebus_dist_cmd_vnode}]),
      ok = riak_core_node_watcher:service_up(ebus, self()),
      ok = riak_core_ring_events:add_guarded_handler(
        ebus_dist_ring_event_handler, []),
      ok = riak_core_node_watcher_events:add_guarded_handler(
        ebus_dist_node_event_handler, []),
      ok;
    _ ->
      ok
  end.
