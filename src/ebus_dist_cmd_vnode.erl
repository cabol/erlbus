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
%%% @doc Riak Core vnode for EBus commands.
%%%-------------------------------------------------------------------
-module(ebus_dist_cmd_vnode).

-ifdef(EBUS_DIST).
-behaviour(riak_core_vnode).
-endif.

%% API
-export([cmd/3]).

%% riak_core_vnode API
-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% VNode State
-record(state, {partition}).

%% Master vnode name
-define(MASTER, ebus_dist_cmd_vnode_master).

%% Sync Call
-define(sync(PrefList, Command, Master),
  riak_core_vnode_master:sync_command(PrefList, Command, Master)
).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the vnode.
-spec start_vnode(any()) -> {ok, pid()}.
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

%% @doc cmd/3.
%% Execute the given command applying the callback function. This
%% callback can be an operation like:
%% <code>reg|unreg|send|sub|unsub|pub</code>
%% <br/>
%% <li>PrefList: Riak Core preflist.</li>
%% <li>Identity: Any value to identify the command.</li>
%% <li>CallbackSpec: Callback specification. This will applied when
%% messages arrives. If `Mod' is `none', the callback will be treated
%% as a fun.</li>
-spec cmd(any(), any(), ebus:callback()) -> term().
cmd(PrefList, Identity, CallbackSpec) ->
  riak_core_vnode_master:command(
    PrefList,
    {cmd, Identity, CallbackSpec},
    {fsm, undefined, self()},
    ?MASTER).

%%%===================================================================
%%% VNode Callbacks
%%%===================================================================

%% @hidden
init([Partition]) ->
  {ok, #state{partition = Partition}}.

%% @hidden
%% @doc Handle ping command - for verification purposes.
handle_command(ping, _Sender, State) ->
  {reply, {pong, State#state.partition}, State};

%% @hidden
%% @doc Handle received command. Applies the callback function.
handle_command({cmd, {ReqId, _}, {M, F, A}}, _Sender, State) ->
  Reply = case M of
            none when is_function(F) ->
              apply(F, A);
            _ ->
              apply(M, F, A)
          end,
  {reply, {ok, ReqId, Reply}, State}.

%% @hidden
handle_handoff_command(_Message, _Sender, State) ->
  %% Delay a little to naively avoid ID collisions
  timer:sleep(1000),
  {forward, State}.

%% @hidden
handoff_starting(_TargetNode, _State) ->
  {true, _State}.

%% @hidden
handoff_cancelled(State) ->
  {ok, State}.

%% @hidden
handoff_finished(_TargetNode, State) ->
  {ok, State}.

%% @hidden
handle_handoff_data(_Data, State) ->
  {reply, ok, State}.

%% @hidden
encode_handoff_item(_ObjectName, _ObjectValue) ->
  <<>>.

%% @hidden
is_empty(State) ->
  {true, State}.

%% @hidden
delete(State) ->
  {ok, State}.

%% @hidden
handle_coverage(_Req, _KeySpaces, _Sender, State) ->
  {stop, not_implemented, State}.

%% @hidden
handle_exit(_Pid, Reason, State) ->
  {stop, Reason, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.
