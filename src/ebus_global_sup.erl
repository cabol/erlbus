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
%%% @doc EBus Global Supervisor.
%%%-------------------------------------------------------------------
-module(ebus_global_sup).

-behaviour(supervisor).

%% Public API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts ebus supervisor.
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

%% @private
init([]) ->
  {ok, {{one_for_one, 5, 10}, process_specs()}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Build the process specifications that will be supervised.
-spec process_specs() -> [supervisor:child_spec()].
process_specs() ->
  Dist = case application:get_env(ebus_dist) of
           {ok, _} -> ebus_dist_specs();
           _       -> []
         end,
  EBus = ebus_spec(),
  [EBus | Dist].

%% @private
ebus_spec() ->
  {ebus_sup,
   {ebus_sup, start_link, []},
   permanent,
   infinity,
   supervisor,
   [ebus_sup]}.

%% @private
ebus_dist_specs() ->
  VMaster = {ebus_dist_cmd_vnode_master,
             {riak_core_vnode_master, start_link, [ebus_dist_cmd_vnode]},
             permanent,
             5000,
             worker,
             [riak_core_vnode_master]},
  CmdFSM = {ebus_dist_cmd_fsm_sup,
            {ebus_dist_cmd_fsm_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [ebus_dist_cmd_fsm_sup]},
  [VMaster, CmdFSM].
