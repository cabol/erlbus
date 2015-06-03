-module(my_test_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({Topic, {Id, Payload}}, Context) ->
  ct:print("\e[1;1m[Pid: ~p][Topic: ~p][Msg: ~p][Ctx: ~p]~n\e[0m",
           [self(), Topic, Payload, Context]),
  ets:insert(ebus_test, {ebus_util:build_name([Id, Context]), Payload}).
