-module(ebus_ps_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([
  all/0,
  init_per_testcase/2,
  end_per_testcase/2
]).

%% Tests
-export([t_broadcast_errors/1, t_broadcast/1]).

%% API
-export([broadcast/4]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() -> [
  t_broadcast_errors,
  t_broadcast
].

init_per_testcase(_, Config) ->
  {ok, Pid} = ebus_ps_pg2:start_link(?MODULE, [{pool_size, 1}]),
  register(ebus_ps_test_subscriber, self()),
  [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
  exit(ebus_common:keyfind(pid, Config), normal),
  timer:sleep(1000),
  Config.

broadcast(Error, _, _, _) ->
  {error, Error}.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

t_broadcast_errors(_Config) ->
  ets:new(failed_broadcaster, [named_table]),
  ets:insert(failed_broadcaster, {broadcast, ?MODULE, [boom]}),

  {error, boom} = ebus_ps:broadcast(failed_broadcaster, <<"hello">>, #{}),

  {error, boom} = ebus_ps:broadcast_from(
    failed_broadcaster, self(), <<"topic">>, ping
  ),

  ct:print("\e[1;1m t_broadcast_errors: \e[0m\e[32m[OK] \e[0m"),
  ok.

t_broadcast(_Config) ->
  SomeSubscriber = spawn_link(fun() -> timer:sleep(infinity) end),
  FastlanePid = spawn_link(fun() -> timer:sleep(infinity) end),
  OtherSubscriber = spawn_link(fun() -> timer:sleep(infinity) end),

  ebus_ps:subscribe(
    ?MODULE, SomeSubscriber, <<"topic1">>,
    [{fastlane, {FastlanePid, ebus_test_serializer, [<<"intercepted">>]}}]
  ),
  ebus_ps:subscribe(
    ?MODULE, self(), <<"topic1">>,
    [{fastlane, {FastlanePid, ebus_test_serializer, [<<"intercepted">>]}}]
  ),
  ebus_ps:subscribe(?MODULE, OtherSubscriber, <<"topic1">>, [{fastlane, nil}]),

  ebus_ps:broadcast(
    ?MODULE, <<"topic1">>,
    ebus_broadcast:new(<<"topic1">>, <<"fastlaned">>, #{})
  ),

  Fastlaned = ebus_message:new(<<"topic1">>, <<"fastlaned">>, #{}),
  {fastlaned, #{ebus_t := broadcast}} = ebus_proc:wait_for_msg(),
  [Fastlaned, Fastlaned] = ebus_proc:messages(FastlanePid),
  [] = ebus_proc:messages(self()),
  [#{ebus_t := broadcast}] = ebus_proc:messages(OtherSubscriber),

  Intercepted = ebus_broadcast:new(<<"topic1">>, <<"intercepted">>, #{}),
  ebus_ps:broadcast(?MODULE, <<"topic1">>, Intercepted),

  Intercepted = ebus_proc:wait_for_msg(5000),
  [Fastlaned, Fastlaned] = ebus_proc:messages(FastlanePid),
  [] = ebus_proc:messages(self()),

  ebus_ps:broadcast_from(
    ?MODULE, self(), <<"topic1">>,
    ebus_broadcast:new(<<"topic1">>, <<"other">>, #{})
  ),

  {fastlaned, #{event := <<"other">>, ebus_t := broadcast}} =
    ebus_proc:wait_for_msg(5000),

  {error, timeout} = ebus_proc:wait_for_msg(1),

  ct:print("\e[1;1m t_broadcast: \e[0m\e[32m[OK] \e[0m"),
  ok.
