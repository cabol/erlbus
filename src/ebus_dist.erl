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
%%% @doc EBus distributed, with Gproc local and Riak Core.
%%%-------------------------------------------------------------------
-module(ebus_dist).

-behaviour(ebus).

%% API
-export([sub/2, unsub/2, pub/2]).
-export([get_subscribers/1, get_topics/0]).
-export([dispatch/3]).

%% Extended API
-export([sub/3, unsub/3, pub/3]).
-export([get_subscribers/2, get_topics/1]).
-export([dispatch/4]).

%% Debug
-export([ping/0, get_dbg_preflist/2, get_dbg_preflist/3]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% Distribution parameters
%% n: number of replicas.
%% sub | unsub | pub | dispatch | get_subscribers | get_topics: quorums.
-type parameter() :: n | sub | unsub | pub | dispatch |
                     get_subscribers | get_topics.

%% ebus_dist options
-type option()    :: {parameter(), pos_integer()}.
-type options()   :: [option()].

%% Exported types
-export_type([parameter/0, option/0, options/0]).

%% Master vnode name
-define(MASTER, ebus_dist_cmd_vnode_master).

%% Riak Core
-define(APP, ebus).
-define(N, 3).

-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

-spec sub(ebus:topic(), ebus:handler()) -> ebus:ebus_ret().
sub(Topic, Handler) ->
  sub(Topic, Handler, []).

-spec sub(ebus:topic(), ebus:handler(), options()) -> ebus:ebus_ret().
sub(Topic, Handler, Opts) ->
  Callback = {ebus_gproc, sub, [Topic, Handler]},
  do_write(Topic, Topic, sub, Callback, Opts).

-spec unsub(ebus:topic(), ebus:handler()) -> any().
unsub(Topic, Handler) ->
  unsub(Topic, Handler, []).

-spec unsub(ebus:topic(), ebus:handler(), options()) -> ebus:ebus_ret().
unsub(Topic, Handler, Opts) ->
  Callback = {ebus_gproc, unsub, [Topic, Handler]},
  do_write(Topic, Topic, unsub, Callback, Opts).

-spec pub(ebus:topic(), ebus:payload()) -> ebus:ebus_ret().
pub(Topic, Message) ->
  pub(Topic, Message, []).

-spec pub(ebus:topic(), ebus:payload(), options()) -> ebus:ebus_ret().
pub(Topic, Message, Opts) ->
  Callback = {ebus_gproc, pub, [Topic, Message]},
  do_write(Topic, Topic, pub, Callback, Opts).

-spec get_subscribers(ebus:topic()) -> ebus:ebus_ret().
get_subscribers(Topic) ->
  get_subscribers(Topic, []).

-spec get_subscribers(ebus:topic(), options()) -> ebus:ebus_ret().
get_subscribers(Topic, Opts) ->
  Callback = {ebus_gproc, get_subscribers, [Topic]},
  do_write(Topic, Topic, get_subscribers, Callback, Opts).

-spec get_topics() -> [ebus:topic()].
get_topics() ->
  get_subscribers([]).

-spec get_topics(options()) -> [ebus:topic()].
get_topics(Opts) ->
  Callback = {ebus_gproc, get_topics, []},
  do_write(undefined, undefined, get_topics, Callback, Opts).

-spec dispatch(
  ebus:topic(), ebus:payload(), ebus:handler()
) -> ebus:ebus_ret().
dispatch(Topic, Message, Handler) ->
  dispatch(Topic, Message, Handler, []).

-spec dispatch(
  ebus:topic(), ebus:payload(), ebus:handler(), options()
) -> ebus:ebus_ret().
dispatch(Topic, Message, Handler, Opts) ->
  Callback = {ebus_gproc, dispatch, [Topic, Message, Handler]},
  do_write(Topic, Topic, dispatch, Callback, Opts).

%%%===================================================================
%%% Debug functions
%%%===================================================================

%% @doc Pings a random vnode to make sure communication is functional.
%% @spec ping() -> term()
-spec ping() -> pong | pang.
ping() ->
  DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, ?APP),
  [{IdxNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IdxNode, ping, ?MASTER).

%% @doc
%% Gets the preflist with default number of nodes (replicas).
%% @equiv get_dbg_preflist(Bucket, Key, ?N)
-spec get_dbg_preflist(binary(), binary()) -> riak_core_apl:preflist().
get_dbg_preflist(Bucket, Key) ->
  get_dbg_preflist(Bucket, Key, ?N).

%% @doc
%% Same as previous but it can receive the number of replicas (nodes).
%% <li>Bucket: Bucket to calculate hash (Riak Core).</li>
%% <li>Key: Key to calculate hash (Riak Core).</li>
%% <li>N: Number of replicas.</li>
-spec get_dbg_preflist(
  binary(), binary(), non_neg_integer()
) -> riak_core_apl:preflist().
get_dbg_preflist(Bucket, Key, N) ->
  DocIdx = riak_core_util:chash_key(
    {ebus_util:to_bin(Bucket), ebus_util:to_bin(Key)}),
  riak_core_apl:get_apl(DocIdx, N, ?APP).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Execute the command against the FSM.
do_write(Bucket, Key, Op, Val, Opts) ->
  BBucket = ebus_util:to_bin(Bucket),
  BKey = ebus_util:to_bin(Key),
  {ok, ReqID} = ebus_dist_cmd_fsm:cmd(BBucket, BKey, Op, Val, Opts),
  wait_for_reqid(ReqID, ?TIMEOUT).

%% @private
%% @doc Waits for the FMS response.
wait_for_reqid(ReqID, Timeout) ->
  receive
    {ok, ReqID} ->
      ok;
    {ok, ReqID, Reply} ->
      Reply
  after Timeout ->
    {error, timeout}
  end.
