%% Feel free to use, reuse and abuse the code in this file.

-module(point_to_point).

-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/0, stop/1]).

-define(TOPIC, <<"P2P_TOPIC">>).

%% API.

start() ->
  application:ensure_all_started(point_to_point).

stop() ->
  application:stop(point_to_point).

start(_Type, _Args) ->
  P = spawn_link(fun() -> dispatcher(?TOPIC) end),
  lists:foreach(fun(N) -> subscriber(?TOPIC, N) end, lists:seq(1, 3)),
  timer:sleep(1 * 60 * 1000),
  exit(P, kill),
  teardown_ebus().

stop(_State) ->
  ok.

%% Internals

dispatcher(Topic) ->
  timer:sleep(500),
  Msg = #{topic => Topic, payload => os:timestamp()},
  io:format("Dispatch: ~p~n", [Msg]),
  ebus:dispatch(Topic, Msg),
  dispatcher(Topic).

subscriber(Topic, N) ->
  Callback = fun(Msg, Args) ->
    io:format(
      "\e[36m---> [PID: ~p][MSG: ~p][Args: ~p]~n\e[0m",
      [self(), Msg, Args]
    )
  end,
  {Handler, _} = ebus_proc:spawn_handler(Callback, [N], [monitor]),
  ebus:sub(Handler, Topic).

teardown_ebus() -> ebus:stop().
