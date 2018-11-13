-module(ebus_dist_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Tests
-export([t_pubsub/1, t_dispatch/1]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() -> [t_pubsub, t_dispatch].

init_per_suite(Config) ->
  ebus:start(),
  Nodes = start_slaves([a, b, c, d]),
  [{nodes, Nodes} | Config].

end_per_suite(Config) ->
  ebus:stop(),
  stop_slaves([a, b, c, d]),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

t_pubsub(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  #{nodes := Nodes} = ConfigMap,

  % check topics
  [] = ebus:local_topics(),
  [] = ebus:topics(),

  % subscribe local process
  ok = ebus:sub(self(), <<"T1">>),

  % spawn and subscribe remote process
  NodePidL = spawn_remote_pids(Nodes),
  sub_remote_pids(NodePidL, <<"T1">>),

  % check topics
  [<<"T1">>] = ebus:local_topics(),
  [<<"T1">>] = ebus:topics(),

  % publish message
  ebus:pub(<<"T1">>, <<"hello">>),
  timer:sleep(1000),

  % check local process received message
  <<"hello">> = ebus_proc:wait_for_msg(5000),
  [] = ebus_proc:messages(self()),

  % check remote processes received message
  L1 = lists:duplicate(4, [<<"hello">>]),
  L1 = r_process_messages(NodePidL),

  % publish message
  ebus:pub_from(self(), <<"T1">>, <<"hello">>),
  timer:sleep(1000),

  % check local process didn't receive message
  [] = ebus_proc:messages(self()),

  % check remote processes received message
  L2 = lists:duplicate(4, [<<"hello">>, <<"hello">>]),
  L2 = r_process_messages(NodePidL),

  % check subscribers
  Self = self(),
  [Self] = ebus:local_subscribers(<<"T1">>),
  5 = length(ebus:subscribers(<<"T1">>)),

  % unsubscribe remote process
  [P1 | _] = NodePidL,
  unsub_remote_pids([P1], <<"T1">>),

  % publish message
  ebus:pub(<<"T1">>, <<"hello">>),
  timer:sleep(1000),

  % check
  <<"hello">> = ebus_proc:wait_for_msg(5000),
  [] = ebus_proc:messages(self()),
  L3 = [
    [<<"hello">>, <<"hello">>]
    | lists:duplicate(3, [<<"hello">>, <<"hello">>, <<"hello">>])
  ],
  L3 = r_process_messages(NodePidL),

  % check subscribers
  [Self] = ebus:local_subscribers(<<"T1">>),
  4 = length(ebus:subscribers(<<"T1">>)),

  % subscribe local process
  ok = ebus:sub(self(), <<"T2">>),

  % publish message
  ebus:pub(<<"T2">>, <<"foo">>),
  timer:sleep(1000),

  % check
  <<"foo">> = ebus_proc:wait_for_msg(5000),
  [] = ebus_proc:messages(self()),
  L3 = r_process_messages(NodePidL),

  % check topics
  [<<"T1">>, <<"T2">>] = ebus:local_topics(),
  [<<"T1">>, <<"T2">>] = ebus:topics(),

  % kill remote pid and check
  {LastNode, LastPid} = lists:last(NodePidL),
  rpc:call(LastNode, erlang, exit, [LastPid, kill]),
  3 = length(ebus:subscribers(<<"T1">>)),

  ct:print("\e[1;1m t_pubsub: \e[0m\e[32m[OK] \e[0m"),
  ok.

t_dispatch(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  #{nodes := Nodes} = ConfigMap,

  % check dispatch
  try ebus:dispatch("foo", <<"M1">>)
  catch _:no_subscribers_available -> ok
  end,

  % subscribe local process
  ok = ebus:sub(self(), "foo"),

  % spawn and subscribe remote process
  NodePidL = spawn_remote_pids(Nodes),
  sub_remote_pids(NodePidL, "foo"),

  1 = length(ebus:local_subscribers("foo")),
  5 = length(ebus:subscribers("foo")),

  % dispatch
  ok = ebus:dispatch("foo", <<"M1">>),
  timer:sleep(1000),

  % check local process received message
  <<"M1">> = ebus_proc:wait_for_msg(5000),
  [] = ebus_proc:messages(self()),

  % check remote processes received message
  L1 = lists:duplicate(4, []),
  L1 = r_process_messages(NodePidL),

  % dispatch global with default dispatch_fun
  lists:foreach(fun(_) ->
    ok = ebus:dispatch("foo", <<"M2">>, [{scope, global}])
  end, lists:seq(1, 500)),
  timer:sleep(1500),

  % check remote processes received message
  % from 500 sent messages, each remote process should have received
  % at least one message
  L2 = r_process_messages(NodePidL),
  lists:foreach(fun(L) -> true = length(L) > 0 end, L2),

  % dispatch fun
  [S1 | _] = ebus:subscribers("foo"),
  MsgsS1 = length(ebus_proc:r_messages(S1)),
  Fun = fun([H | _]) -> H end,
  lists:foreach(fun(_) ->
    ok = ebus:dispatch(
      "foo", <<"M3">>,
      [{scope, global}, {dispatch_fun, Fun}])
  end, lists:seq(1, 100)),
  timer:sleep(1500),
  MsgsS11 = MsgsS1 + 100,
  MsgsS11 = length(ebus_proc:r_messages(S1)),

  ct:print("\e[1;1m t_dispatch: \e[0m\e[32m[OK] \e[0m"),
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

start_slaves(Slaves) ->
  start_slaves(Slaves, []).

start_slaves([], Acc) ->
  lists:usort(Acc);
start_slaves([Node | T], Acc) ->
  ErlFlags = "-pa ../../lib/*/ebin -config ../../../../config/test.config",
  {ok, HostNode} = ct_slave:start(Node, [
    {kill_if_fail, true},
    {monitor_master, true},
    {init_timeout, 3000},
    {startup_timeout, 3000},
    {startup_functions, [{ebus, start, []}]},
    {erl_flags, ErlFlags}
  ]),
  ct:print("\e[36m ---> Node ~p \e[32m[OK] \e[0m", [HostNode]),
  pong = net_adm:ping(HostNode),
  start_slaves(T, [HostNode | Acc]).

stop_slaves(Slaves) ->
  stop_slaves(Slaves, []).

stop_slaves([], Acc) ->
  lists:usort(Acc);
stop_slaves([Node | T], Acc) ->
  {ok, Name} = ct_slave:stop(Node),
  ct:print("\e[36m ---> Node ~p \e[31m[STOPPED] \e[0m", [Name]),
  pang = net_adm:ping(Node),
  stop_slaves(T, [Node | Acc]).

spawn_remote_pids(RemoteNodes) ->
  {ResL, _} = rpc:multicall(
    RemoteNodes, ebus_proc, spawn_timer_fun, [infinity]
  ),
  lists:zip(RemoteNodes, ResL).

sub_remote_pids(RemotePids, Topic) ->
  lists:foreach(fun({Node, Pid}) ->
    ok = rpc:call(Node, ebus, sub, [Pid, Topic])
  end, RemotePids).

unsub_remote_pids(RemotePids, Topic) ->
  lists:foreach(fun({Node, Pid}) ->
    ok = rpc:call(Node, ebus, unsub, [Pid, Topic])
  end, RemotePids).

r_process_messages(RemotePids) ->
  [ebus_proc:r_messages(Pid) || {_, Pid} <- RemotePids].
