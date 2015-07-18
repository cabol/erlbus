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
%%% @doc Test suite for ebus Local.
%%%-------------------------------------------------------------------
-module(ebus_local_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Tests
-export([ebus_pub_sub/1, ebus_pool/1]).

-define(TAB, ebus_test).
-define(HANDLER, my_test_handler).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [ebus_pub_sub, ebus_pool].

init_per_suite(Config) ->
  application:start(ebus),
  Config.

end_per_suite(Config) ->
  application:stop(ebus),
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

ebus_pub_sub(_Config) ->
  %% Debug
  ct:print("\e[1;96m 'ebus_pub_sub' testcase. \e[0m"),

  %% Clean ETS table
  true = ets:delete_all_objects(?TAB),

  %% Create handlers
  MH1 = ebus_handler:new(?HANDLER, <<"MH1">>),
  MH2 = ebus_handler:new(?HANDLER, <<"MH2">>),
  MH3 = ebus_handler:new(?HANDLER, <<"MH3">>),

  %% Subscribe MH1 and MH2
  ok = ebus:sub(ch1, [MH1, MH2]),

  %% Subscribe MH1 and MH2 to other channels
  ok = ebus:sub(ch2, MH1),
  ok = ebus:sub(ch3, MH2),

  %% Check subscribers
  2 = length(ebus:get_subscribers(ch1)),

  %% Check channels
  3 = length(ebus:get_channels()),

  %% Publish to 'ch1'
  ok = ebus:pub(ch1, {<<"ID1">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH1, MH2)
  [{_, M11}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1">>, <<"MH1">>])),
  M11 = <<"Hi!">>,
  [{_, M12}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1">>, <<"MH2">>])),
  M12 = <<"Hi!">>,
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1">>, <<"MH3">>])),

  %% Publish to 'ch2'
  ok = ebus:pub(ch2, {<<"ID1-2">>, <<"Hi!">>}),
  timer:sleep(500),
  [{_, M13}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1-2">>, <<"MH1">>])),
  M13 = <<"Hi!">>,
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1-2">>, <<"MH2">>])),
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1-2">>, <<"MH3">>])),

  %% Publish to 'ch3'
  ok = ebus:pub(ch3, {<<"ID1-3">>, <<"Hi!">>}),
  timer:sleep(500),
  [{_, M14}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1-3">>, <<"MH2">>])),
  M14 = <<"Hi!">>,
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1-3">>, <<"MH1">>])),
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1-3">>, <<"MH3">>])),

  %% Subscribe MH3
  ok = ebus:sub(ch1, MH3),

  %% Check subscribers
  3 = length(ebus:get_subscribers(ch1)),

  %% Publish to 'ch1'
  ok = ebus:pub(ch1, {<<"ID2">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH1, MH2, MH3)
  [{_, M21}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID2">>, <<"MH1">>])),
  M21 = <<"Hi!">>,
  [{_, M22}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID2">>, <<"MH2">>])),
  M22 = <<"Hi!">>,
  [{_, M23}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID2">>, <<"MH3">>])),
  M23 = <<"Hi!">>,

  %% Send to 'ch1' and 'MH1'
  ok = ebus:dispatch(ch1, {<<"ID2-1">>, <<"Send">>}, MH1),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH1, MH2, MH3)
  [{_, M21_2}] = ets:lookup(
    ?TAB, ebus_util:build_name([<<"ID2-1">>, <<"MH1">>])),
  M21_2 = <<"Send">>,

  %% Unsubscribe MH1 and MH2
  ok = ebus:unsub(ch1, [MH1, MH2]),

  %% Check subscribers
  1 = length(ebus:get_subscribers(ch1)),

  %% Publish to 'ch1'
  ok = ebus:pub(ch1, {<<"ID3">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH3)
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID3">>, <<"MH1">>])),
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID3">>, <<"MH2">>])),
  [{_, M33}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID3">>, <<"MH3">>])),
  M33 = <<"Hi!">>,

  %% Unsubscribe MH1 from 'ch2'
  ok = ebus:unsub(ch2, MH1),

  %% Check subscribers
  0 = length(ebus:get_subscribers(ch2)),

  %% Publish to 'ch2'
  ok = ebus:pub(ch2, {<<"ID4">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH3)
  [] = ets:lookup(?TAB, ebus_util:build_name([<<"ID4">>, <<"MH1">>])),

  %% End
  cleanup([MH1, MH2, MH3]),
  ok.

ebus_pool(_Config) ->
  %% Debug
  ct:print("\e[1;96m 'ebus_pool' testcase. \e[0m"),

  %% Clean ETS table
  true = ets:delete_all_objects(?TAB),

  %% Create ebus instance
  Name = ebus_util:build_name([erlang:phash2(os:timestamp())]),
  {ok, _} = ebus:new(Name),

  %% Create handlers
  MH1 = ebus_handler:new(?HANDLER, <<"MH1">>),
  MH2 = ebus_handler:new_pool(my_pool_1, 3, ?HANDLER, <<"MH2">>),

  %% Subscribe MH1 and MH2
  ok = ebus:sub(Name, ch1, MH1),
  ok = ebus:sub(Name, ch1, MH2),

  %% Check subscribers
  2 = length(ebus:get_subscribers(Name, ch1)),

  %% Publish to 'ch1'
  ok = ebus:pub(Name, ch1, {<<"ID1">>, <<"Hi!">>}),
  timer:sleep(500),

  %% Check arrival of messages to right handlers (MH1, MH2)
  [{_, M11}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1">>, <<"MH1">>])),
  M11 = <<"Hi!">>,
  [{_, M12}] = ets:lookup(?TAB, ebus_util:build_name([<<"ID1">>, <<"MH2">>])),
  M12 = <<"Hi!">>,

  %% End
  cleanup([MH1, MH2]),
  ok.

%%%===================================================================
%%% Internals
%%%===================================================================

cleanup(Handlers) ->
  [ebus_handler:delete(Handler) || Handler <- Handlers].
