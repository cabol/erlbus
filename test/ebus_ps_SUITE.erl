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

-include_lib("ebus/include/ebus.hrl").

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
  exit(ebus_utils:keyfind(pid, Config), normal),
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
    ebus_broadcast:from_map(
      #{event => <<"fastlaned">>, topic => <<"topic1">>, payload => #{}}
    )
  ),

  Fastlaned = ebus_message:from_map(
    #{event => <<"fastlaned">>, topic => <<"topic1">>, payload => #{}}
  ),
  {fastlaned, #broadcast{}} = ebus_process:wait_for_msg(5000),
  [Fastlaned, Fastlaned] = ebus_process:messages(FastlanePid),
  [] = ebus_process:messages(self()),
  [#broadcast{}] = ebus_process:messages(OtherSubscriber),

  ebus_ps:broadcast(
    ?MODULE, <<"topic1">>,
    ebus_broadcast:from_map(
      #{event => <<"intercepted">>, topic => <<"topic1">>, payload => #{}}
    )
  ),

  #broadcast{event = <<"intercepted">>, topic = <<"topic1">>, payload = #{}} =
    ebus_process:wait_for_msg(5000),
  [Fastlaned, Fastlaned] = ebus_process:messages(FastlanePid),
  [] = ebus_process:messages(self()),

  ebus_ps:broadcast_from(
    ?MODULE, self(), <<"topic1">>,
    ebus_broadcast:from_map(
      #{event => <<"other">>, topic => <<"topic1">>, payload => #{}}
    )
  ),

  {fastlaned, #broadcast{event = <<"other">>}} =
    ebus_process:wait_for_msg(5000),

  ct:print("\e[1;1m t_broadcast: \e[0m\e[32m[OK] \e[0m"),
  ok.
