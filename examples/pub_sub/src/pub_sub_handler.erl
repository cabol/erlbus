%% Feel free to use, reuse and abuse the code in this file.

%% @doc handler.
-module(pub_sub_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({Channel, Msg}, Context) ->
  io:format("[Pid: ~p][Channel: ~p][Msg: ~p][Ctx: ~p]~n",
            [self(), Channel, Msg, Context]).
