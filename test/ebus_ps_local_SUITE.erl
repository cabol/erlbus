-module(ebus_ps_local_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Tests
-export([
  t_subscribe/1,
  t_unsubscribe/1,
  t_unsubscribe2/1,
  t_pid_removed_when_down/1,
  t_subscriber_demonitored/1
]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() -> [
  t_subscribe,
  t_unsubscribe,
  t_unsubscribe2,
  t_pid_removed_when_down,
  t_subscriber_demonitored
].

init_per_suite(Config) ->
  ebus:start(),
  PubSub = application:get_env(ebus, pubsub, []),
  Config ++ PubSub.

end_per_suite(Config) ->
  ebus:stop(),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

t_subscribe(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  #{name := PubSub, pool_size := PoolSize} = ConfigMap,

  % subscribe
  Pid = spawn_link(fun() -> timer:sleep(infinity) end),
  [] = ebus_ps_local:subscribers(PubSub, PoolSize, <<"foo">>),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, self(), <<"foo">>),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, Pid, <<"foo">>),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, self(), <<"bar">>),

  % broadcast
  ok = ebus_ps_local:broadcast(PubSub, PoolSize, none, <<"foo">>, hellofoo),
  hellofoo = ebus_proc:wait_for_msg(5000),
  [hellofoo] = ebus_proc:messages(Pid),

  ok = ebus_ps_local:broadcast(PubSub, PoolSize, none, <<"bar">>, hellobar),
  hellobar = ebus_proc:wait_for_msg(5000),
  [hellofoo] = ebus_proc:messages(Pid),

  ok = ebus_ps_local:broadcast(PubSub, PoolSize, none, <<"unknown">>, hellobar),
  [] = ebus_proc:messages(self()),

  ok = ebus_ps_local:broadcast(PubSub, PoolSize, self(), <<"foo">>, hellofoo),
  [hellofoo, hellofoo] = ebus_proc:messages(Pid),
  [] = ebus_proc:messages(self()),

  ct:print("\e[1;1m t_subscribe: \e[0m\e[32m[OK] \e[0m"),
  ok.

t_unsubscribe(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  #{name := PubSub, pool_size := PoolSize} = ConfigMap,

  Pid = spawn_link(fun() -> timer:sleep(infinity) end),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, self(), <<"topic1">>),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, Pid, <<"topic1">>),

  SubL1 = lists:sort(
    ebus_ps_local:subscribers(PubSub, PoolSize, <<"topic1">>)),
  SubL2 = lists:sort([self(), Pid]),
  SubL1 = SubL2,
  ok = ebus_ps_local:unsubscribe(PubSub, PoolSize, self(), <<"topic1">>),
  [Pid] = ebus_ps_local:subscribers(PubSub, PoolSize, <<"topic1">>),

  ok = ebus_ps_local:broadcast(PubSub, PoolSize, none, <<"topic1">>, foo),
  [] = ebus_proc:messages(),
  [foo] = ebus_proc:messages(Pid),

  ct:print("\e[1;1m t_unsubscribe: \e[0m\e[32m[OK] \e[0m"),
  ok.

t_unsubscribe2(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  #{name := PubSub, pool_size := PoolSize} = ConfigMap,

  % unsubscribe GC collect topic when there are no more subscribers
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, self(), <<"topic1">>),
  [<<"topic1">>] = ebus_ps_local:list(PubSub, PoolSize),
  ok = ebus_ps_local:unsubscribe(PubSub, PoolSize, self(), <<"topic1">>),
  0 = length(ebus_ps_local:list(PubSub, PoolSize)),
  0 = length(ebus_ps_local:subscribers(PubSub, PoolSize, <<"topic1">>)),

  % unsubscribe when topic does not exists
  ok = ebus_ps_local:unsubscribe(PubSub, PoolSize, self(), <<"notexists">>),
  0 = length(ebus_ps_local:subscribers(PubSub, PoolSize, <<"notexists">>)),

  ct:print("\e[1;1m t_unsubscribe2: \e[0m\e[32m[OK] \e[0m"),
  ok.

t_pid_removed_when_down(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  #{name := PubSub, pool_size := PoolSize} = ConfigMap,

  {Pid, Ref} = spawn_monitor(fun() -> timer:sleep(infinity) end),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, self(), <<"topic5">>),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, Pid, <<"topic5">>),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, Pid, <<"topic6">>),

  exit(Pid, kill),
  {'DOWN', Ref, _, _, _} = ebus_proc:wait_for_msg(5000),

  % Ensure DOWN is processed to avoid races
  ebus_ps_local:subscribe(PubSub, PoolSize, Pid, <<"unknown">>),
  ebus_ps_local:unsubscribe(PubSub, PoolSize, Pid, <<"unknown">>),

  [] = ebus_ps_local:subscription(PubSub, PoolSize, Pid),
  Self = self(),
  [Self] = ebus_ps_local:subscribers(PubSub, PoolSize, <<"topic5">>),
  [] = ebus_ps_local:subscribers(PubSub, PoolSize, <<"topic6">>),

  % Assert topic was also garbage collected
  [<<"topic5">>] = ebus_ps_local:list(PubSub, PoolSize),

  ct:print("\e[1;1m t_pid_removed_when_down: \e[0m\e[32m[OK] \e[0m"),
  ok.

t_subscriber_demonitored(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  #{name := PubSub, pool_size := PoolSize} = ConfigMap,

  ok = ebus_ps_local:subscribe(PubSub, PoolSize, self(), <<"topic7">>),
  ok = ebus_ps_local:subscribe(PubSub, PoolSize, self(), <<"topic8">>),

  Topics1 = ebus_ps_local:subscription(PubSub, PoolSize, self()),
  [<<"topic7">>, <<"topic8">>] = lists:sort(Topics1),

  ok = ebus_ps_local:unsubscribe(PubSub, PoolSize, self(), <<"topic7">>),
  Topics2 = ebus_ps_local:subscription(PubSub, PoolSize, self()),
  [<<"topic8">>] = lists:sort(Topics2),

  ok = ebus_ps_local:unsubscribe(PubSub, PoolSize, self(), <<"topic8">>),
  [] = ebus_ps_local:subscription(PubSub, PoolSize, self()),

  ct:print("\e[1;1m t_subscriber_demonitored: \e[0m\e[32m[OK] \e[0m"),
  ok.
