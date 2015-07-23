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
-export([subscribers/1, channels/0]).
-export([dispatch/3]).

%% Extended API
-export([sub/3, unsub/3, pub/3]).
-export([subscribers/2, channels/1]).
-export([dispatch/4]).

%% Debug
-export([ping/0, get_dbg_preflist/2, get_dbg_preflist/3]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% Distribution parameters
%% n: number of replicas.
%% sub | unsub | pub | dispatch | subscribers | channels: quorums.
-type parameter() :: n | sub | unsub | pub | dispatch |
                     subscribers | channels.

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

-spec sub(ebus:channel(), ebus:handler()) -> ebus:ebus_ret().
sub(Channel, Handler) ->
  sub(Channel, Handler, []).

-spec sub(ebus:channel(), ebus:handler(), options()) -> ebus:ebus_ret().
sub(Channel, Handler, Opts) ->
  Callback = {ebus_gproc, sub, [Channel, Handler]},
  do_write(Channel, Channel, sub, Callback, Opts).

-spec unsub(ebus:channel(), ebus:handler()) -> ebus:ebus_ret().
unsub(Channel, Handler) ->
  unsub(Channel, Handler, []).

-spec unsub(ebus:channel(), ebus:handler(), options()) -> ebus:ebus_ret().
unsub(Channel, Handler, Opts) ->
  Callback = {ebus_gproc, unsub, [Channel, Handler]},
  do_write(Channel, Channel, unsub, Callback, Opts).

-spec pub(ebus:channel(), ebus:payload()) -> ebus:ebus_ret().
pub(Channel, Message) ->
  pub(Channel, Message, []).

-spec pub(ebus:channel(), ebus:payload(), options()) -> ebus:ebus_ret().
pub(Channel, Message, Opts) ->
  Callback = {ebus_gproc, pub, [Channel, Message]},
  do_write(Channel, Channel, pub, Callback, Opts).

-spec subscribers(ebus:channel()) -> [ebus:handler()].
subscribers(Channel) ->
  subscribers(Channel, []).

-spec subscribers(ebus:channel(), options()) -> [ebus:handler()].
subscribers(Channel, Opts) ->
  Callback = {ebus_gproc, subscribers, [Channel]},
  do_write(Channel, Channel, subscribers, Callback, Opts).

-spec channels() -> [ebus:channel()].
channels() ->
  channels([]).

-spec channels(options()) -> [ebus:channel()].
channels(Opts) ->
  Callback = {ebus_gproc, channels, []},
  do_write(undefined, undefined, channels, Callback, Opts).

-spec dispatch(
  ebus:channel(), ebus:payload(), ebus:handler()
) -> ebus:ebus_ret().
dispatch(Channel, Message, Handler) ->
  dispatch(Channel, Message, Handler, []).

-spec dispatch(
  ebus:channel(), ebus:payload(), ebus:handler(), options()
) -> ebus:ebus_ret().
dispatch(Channel, Message, Handler, Opts) ->
  Callback = {ebus_gproc, dispatch, [Channel, Message, Handler]},
  do_write(Channel, Channel, dispatch, Callback, Opts).

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
