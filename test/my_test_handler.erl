-module(my_test_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

%% Message Handler
handle_msg({Channel, {Id, Payload} = Msg}, Context) ->
  ct:print("\e[1;1m[Pid: ~p][Channel: ~p][Msg: ~p][Ctx: ~p]~n\e[0m",
           [self(), Channel, Msg, Context]),
  ets:insert(ebus_test, {ebus_util:build_name([Id, Context]), Payload}).
