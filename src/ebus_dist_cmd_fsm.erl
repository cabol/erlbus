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
%%% @doc EBus Coordinator for command opeartions.
%%%-------------------------------------------------------------------
-module(ebus_dist_cmd_fsm).

-behavior(gen_fsm).

%% API
-export([start_link/5, start_link/7, cmd/3, cmd/5]).

%% Callbacks
-export([init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

-type proplist() :: [{any(), any()}].

%% Riak Core
-define(APP, ebus).
-define(N, 1).

%% State
-record(state, {req_id          :: pos_integer(),
                coordinator     :: node(),
                from            :: pid(),
                bkey            :: binary(),
                op              :: atom(),
                val = undefined :: term() | undefined,
                preflist        :: riak_core_apl:preflist2(),
                n               :: non_neg_integer(),
                q               :: non_neg_integer(),
                num_q = 0       :: non_neg_integer(),
                replies = []    :: list()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc start_link/5.
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% <br/>
%% <li>ReqId: The request id so the caller can verify the response.</li>
%% <li>From: The pid of the sender so a reply can be made.</li>
%% <li>Bucket: Bucket to calculate hash.</li>
%% <li>Key: Key to calculate hash.</li>
%% <li>Op: ebus operarion.</li>
%% @equiv start_link(ReqId, From, Bucket, Key, Op, undefined, [])
-spec start_link(
  any(), pid(), binary(), binary(), ebus:cmd()
) -> gen:start_ret().
start_link(ReqId, From, Bucket, Key, Op) ->
  start_link(ReqId, From, Bucket, Key, Op, undefined, []).

%% @doc start_link/7.
%% Same of the previous function but, it can receive value and
%% option list.
%% <br/>
%% <li>ReqId: The request id so the caller can verify the response.</li>
%% <li>From: The pid of the sender so a reply can be made.</li>
%% <li>Bucket: Bucket to calculate hash.</li>
%% <li>Key: Key to calculate hash.</li>
%% <li>Op: ebus operarion.</li>
%% <li>Val: Any value that is passed to FSM.</li>
%% <li>
%% Opts: Option list.
%% q = quorum
%% n = replicas
%% Example: <code>[{q, 1}, {n, 1}]</code>
%% </li>
-spec start_link(
  any(), pid(), binary(), binary(), ebus:cmd(), any(), proplist()
) -> gen:start_ret().
start_link(ReqId, From, Bucket, Key, Op, Val, Opts) ->
  gen_fsm:start_link(?MODULE, [ReqId, From, Bucket, Key, Op, Val, Opts], []).

%% @doc cmd/3.
%% Start the FSM in order to execute the given command.
%% <li>Bucket: Bucket to calculate hash.</li>
%% <li>Key: Key to calculate hash.</li>
%% <li>Op: ebus operarion.</li>
%% @equiv cmd(Bucket, Key, Op, undefined, [])
-spec cmd(binary(), binary(), ebus:cmd()) -> {ok, any()}.
cmd(Bucket, Key, Op) ->
  cmd(Bucket, Key, Op, undefined, []).

%% @doc cmd/5.
%% Same of previous but with value and option list.
%% <li>Bucket: Bucket to calculate hash.</li>
%% <li>Key: Key to calculate hash.</li>
%% <li>Op: ebus operarion.</li>
%% <li>Val: Any value that is passed to FSM.</li>
%% <li>
%% Opts: Option list.
%% Example: <code>[{q, 1}, {n, 1}]</code> where:
%% q = quorum
%% n = replicas
%% </li>
-spec cmd(binary(), binary(), ebus:cmd(), any(), proplist()) -> {ok, any()}.
cmd(Bucket, Key, Op, Val, Opts) ->
  ReqId = mk_reqid(),
  ebus_dist_cmd_fsm_sup:start_child(
    [ReqId, self(), Bucket, Key, Op, Val, Opts]),
  {ok, ReqId}.

%%%===================================================================
%%% Callbacks
%%%===================================================================

%% @hidden
handle_info(_Info, _StateName, StateData) ->
  {stop, badmsg, StateData}.

%% @hidden
handle_event(_Event, _StateName, StateData) ->
  {stop, badmsg, StateData}.

%% @hidden
handle_sync_event(_Event, _From, _StateName, StateData) ->
  {stop, badmsg, StateData}.

%% @hidden
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% @hidden
terminate(_Reason, _SN, _SD) ->
  ok.

%%%===================================================================
%%% States
%%%===================================================================

%% @hidden
init([ReqId, From, Bucket, Key, Op, Val, Opts]) ->
  N = ebus_util:keyfind(n, Opts, ?N),
  Q = ebus_util:keyfind(q, Opts, round(?N/2)),
  SD = #state{req_id = ReqId,
              coordinator = node(),
              from = From,
              bkey = {Bucket, Key},
              op = Op,
              val = Val,
              n = N,
              q = Q},
  {ok, prepare, SD, 0}.

%% @hidden
%% @doc Prepare the write by calculating the _preference list_.
prepare(timeout, SD0 = #state{bkey = BKey, n = N}) ->
  DocIdx = riak_core_util:chash_key(BKey),
  PrefList = riak_core_apl:get_apl(DocIdx, N, ?APP),
  SD = SD0#state{preflist = PrefList},
  {next_state, execute, SD, 0}.

%% @hidden
%% @doc Execute the command request and then go into waiting state to
%%      verify it has meets consistency requirements.
execute(timeout,
        SD0 = #state{req_id = ReqId,
                     coordinator = Coordinator,
                     val = Val,
                     preflist = PrefL}) ->
  ebus_dist_cmd_vnode:cmd(PrefL, {ReqId, Coordinator}, Val),
  {next_state, waiting, SD0}.

%% @hidden
%% @doc Wait for Q write reqs to respond.
waiting({ok, ReqId, Val},
        SD0 = #state{from = From,
                     op = Op,
                     num_q = NumQ0,
                     q = Q,
                     replies = Rep0}) ->
  NumQ = NumQ0 + 1,
  Replies = [Val | Rep0],
  SD = SD0#state{num_q = NumQ, replies = Replies},
  case NumQ =:= Q of
    true ->
      Reply = case lists:any(different(Val), Replies) of
                true  -> repair(Op, Replies, SD);
                false -> Val
              end,
      From ! {ok, ReqId, Reply},
      {stop, normal, SD};
    false ->
      {next_state, waiting, SD}
  end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
mk_reqid() -> erlang:phash2(os:timestamp()).

%% @private
different(A) -> fun(B) -> A =/= B end.

%% @private
repair(get_subscribers, Replies, _State) ->
  max_size_list(Replies);
repair(get_channels, Replies, _State) ->
  MergedList = merge_lists(Replies),
  ebus_util:rem_dups_from_list(MergedList);
repair(_Op, Replies, #state{q = Q}) ->
  case ebus_util:count_val_in_list(ok, Replies) >= round(Q/2) of
    true  -> ok;
    false -> {error, quorum_reconciliation_failed}
  end.

%% @private
max_size_list(L) ->
  F = fun(E, Acc) ->
        case length(E) > length(Acc) of
          true  -> E;
          false -> Acc
        end
      end,
  lists:foldl(F, [], L).

%% @private
merge_lists(L) ->
  F = fun(E, Acc) -> E ++ Acc end,
  lists:foldl(F, [], L).
