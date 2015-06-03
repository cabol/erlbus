-module(test_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({Topic, Msg}, Context) ->
  %%timer:sleep(2000),
  io:format("\e[1;1m[Pid: ~p][Topic: ~p][Msg: ~p][Ctx: ~p]~n\e[0m",
            [self(), Topic, Msg, Context]).
