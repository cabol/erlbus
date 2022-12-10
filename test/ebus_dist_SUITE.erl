-module(ebus_dist_SUITE).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Tests
-export([
  t_pubsub/1,
  t_dispatch/1
]).

-define(SLAVES, [
  'a@127.0.0.1',
  'b@127.0.0.1',
  'c@127.0.0.1',
  'd@127.0.0.1'
]).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() -> [t_pubsub, t_dispatch].

init_per_suite(Config) ->
  {ok, PeerNodes} = ebus_dist_cluster:start(?SLAVES),
  _ = ebus:start(),
  [{nodes, ?SLAVES}, {peer_nodes, PeerNodes} | Config].

end_per_suite(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  PeerNodes = maps:get(peer_nodes, ConfigMap),

  _ = ebus:stop(),
  _ = ebus_dist_cluster:stop(PeerNodes),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

t_pubsub(Config) ->
  % get config properties
  ConfigMap = maps:from_list(Config),
  Nodes = maps:get(nodes, ConfigMap),

  % check topics
  [] = ebus:local_topics(),
  [] = ebus:topics(),

  % subscribe local process
  ok = ebus:sub(self(), <<"T1">>),

  % spawn and subscribe remote process
  NodePidL = spawn_remote_pids(Nodes),
  sub_remote_pids(NodePidL, <<"T1">>),

  ebus_common:wait_until(fun() ->
    length(ebus_ps_pg:get_members(ebus_ps)) =:= 5
  end),

  % check topics
  [<<"T1">>] = ebus:local_topics(),
  [<<"T1">>] = ebus:topics(),

  % publish message
  ebus:pub(<<"T1">>, <<"hello">>),
  timer:sleep(1000),

  % check local process received message
  <<"hello">> = ebus_proc:wait_for_msg(5000),

  % check remote processes received message
  Nodes = wait_for_reemote_msg(Nodes, <<"hello">>),
  [] = ebus_proc:messages(self()),

  % publish message
  ebus:pub_from(self(), <<"T1">>, <<"hello">>),
  timer:sleep(1000),

  % check remote processes received message
  Nodes = wait_for_reemote_msg(Nodes, <<"hello">>),
  [] = ebus_proc:messages(self()),

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

  % check local
  <<"hello">> = ebus_proc:wait_for_msg(5000),

  % check remotes
  Nodes1 = tl(Nodes),
  Nodes1 = wait_for_reemote_msg(Nodes1, <<"hello">>),
  [] = ebus_proc:messages(self()),

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
  Nodes = maps:get(nodes, ConfigMap),

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

  % check remote processes didn't receive message
  [] = ebus_proc:messages(self()),

  % dispatch global with default dispatch_fun
  Enum = lists:seq(1, 50),
  lists:foreach(fun(_) ->
    ok = ebus:dispatch("foo", <<"M2">>, [{scope, global}])
  end, Enum),

  timer:sleep(1500),

  % check processes received at least one message
  Result = wait_for_reemote_msg(Enum, <<"M2">>, 1),
  Result = lists:usort([node() | Nodes]),

  % dispatch fun
  [S1 | _] = ebus:subscribers("foo"),
  ExpectedNode = node(S1),
  Fun = fun([H | _]) -> H end,
  Enum = lists:seq(1, 50),
  lists:foreach(fun(_) ->
    ebus:dispatch(
      "foo", <<"M3">>,
      [{scope, global}, {dispatch_fun, Fun}]
    )
  end, Enum),
  [ExpectedNode] = wait_for_reemote_msg(Enum, <<"M3">>, 1),

  ct:print("\e[1;1m t_dispatch: \e[0m\e[32m[OK] \e[0m"),
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

spawn_remote_pids(RemoteNodes) ->
  Fun = fun(Msg, Pid) ->
    Pid ! {node(), Msg}
  end,

  {ResL, []} = rpc:multicall(RemoteNodes, ebus_proc, spawn_handler, [Fun, [self()]]),
  lists:zip(RemoteNodes, ResL).

sub_remote_pids(RemotePids, Topic) ->
  lists:foreach(fun({Node, Pid}) ->
    ok = rpc:call(Node, ebus, sub, [Pid, Topic])
  end, RemotePids).

unsub_remote_pids(RemotePids, Topic) ->
  lists:foreach(fun({Node, Pid}) ->
    ok = rpc:call(Node, ebus, unsub, [Pid, Topic])
  end, RemotePids).

wait_for_reemote_msg(Enum, Msg) ->
  wait_for_reemote_msg(Enum, Msg, 5000).

wait_for_reemote_msg(Enum, Msg, Timeout) ->
  lists:usort([
    begin
      case ebus_proc:wait_for_msg(Timeout) of
        {Ni, Msg} -> Ni;
        Msg -> node()
      end
    end
    || _Elem <- Enum
  ]).
