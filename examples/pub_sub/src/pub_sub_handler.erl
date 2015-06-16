%% Feel free to use, reuse and abuse the code in this file.

%% @doc handler.
-module(pub_sub_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({Topic, Msg}, Context) ->
	io:format("[Pid: ~p][Topic: ~p][Msg: ~p][Ctx: ~p]~n",
						[self(), Topic, Msg, Context]).
