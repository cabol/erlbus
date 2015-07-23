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
%%% @doc Basic Test suite.
%%%-------------------------------------------------------------------
-module(ebus_pg2_gproc_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Tests
-export([pub_sub/1]).

-define(TAB, ebus_test).
-define(HANDLER, my_test_handler).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [pub_sub].

init_per_suite(Config) ->
  application:start(gproc),
  Config.

end_per_suite(Config) ->
  application:stop(gproc),
  Config.

init_per_testcase(_, Config) ->
  TabId = ets:new(?TAB, [duplicate_bag, public, named_table]),
  [{table,TabId} | Config].

end_per_testcase(_, Config) ->
  ets:delete(?TAB),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

pub_sub(_Config) ->
  run_all_providers(fun t_pub_sub/1).

%%%===================================================================
%%% Internal Tests Functions
%%%===================================================================

t_pub_sub(Module) ->
  %% Debug
  ct:print("\e[1;96m [~p] 'pub_sub' testcase. \e[0m", [Module]),

  %% Clean ETS table
  true = ets:delete_all_objects(?TAB),

  %% Create handlers
  MH1 = ebus_handler:new(?HANDLER, <<"MH1">>),
  MH2 = ebus_handler:new(?HANDLER, <<"MH2">>),
  MH3 = ebus_handler:new(?HANDLER, <<"MH3">>),

  %% Subscribe MH1 and MH2
  ok = Module:sub(ch1, [MH1, MH2]),

  %% Check subscribers
  2 = length(Module:subscribers(ch1)),

  %% Publish to 'ch1'
  ok = Module:pub(ch1, {<<"ID1">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH1, MH2)
  [{_, M11}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1">>, <<"MH1">>])),
  M11 = <<"Hi!">>,
  [{_, M12}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1">>, <<"MH2">>])),
  M12 = <<"Hi!">>,
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1">>, <<"MH3">>])),

  %% Subscribe MH3
  ok = Module:sub(ch1, MH3),

  %% Check subscribers
  3 = length(Module:subscribers(ch1)),

  %% Check channels
  1 = length(Module:channels()),

  %% Publish to 'ch1'
  ok = Module:pub(ch1, {<<"ID2">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH1, MH2, MH3)
  [{_, M21}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID2">>, <<"MH1">>])),
  M21 = <<"Hi!">>,
  [{_, M22}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID2">>, <<"MH2">>])),
  M22 = <<"Hi!">>,
  [{_, M23}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID2">>, <<"MH3">>])),
  M23 = <<"Hi!">>,

  %% Send to 'ch1' and 'MH1'
  ok = Module:dispatch(ch1, {<<"ID2-1">>, <<"Send">>}, MH1),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH1, MH2, MH3)
  [{_, M21_2}] = ets:lookup(
    ?TAB, ebus_util:build_name([<<"ID2-1">>, <<"MH1">>])),
  M21_2 = <<"Send">>,

  %% Unsubscribe MH1 and MH2
  ok = Module:unsub(ch1, [MH1, MH2]),

  %% Check subscribers
  1 = length(Module:subscribers(ch1)),

  %% Publish to 'ch1'
  ok = Module:pub(ch1, {<<"ID3">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH3)
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID3">>, <<"MH1">>])),
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID3">>, <<"MH2">>])),
  [{_, M33}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID3">>, <<"MH3">>])),
  M33 = <<"Hi!">>,

  %% Unsubscribe MH3
  ok = Module:unsub(ch1, MH3),

  %% Publish to 'ch1'
  ok = Module:pub(ch1, {<<"ID3-1">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to MH3
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID3-1">>, <<"MH3">>])),

  %% Check subscribers
  0 = length(Module:subscribers(ch1)),

  %% End
  cleanup([MH1, MH2, MH3]),
  ok.

%%%===================================================================
%%% Internals
%%%===================================================================

run_all_providers(Fun) ->
  Modules = [ebus_pg2, ebus_gproc],
  lists:foreach(Fun, Modules).

cleanup(Handlers) ->
  [ebus_handler:delete(Handler) || Handler <- Handlers].
