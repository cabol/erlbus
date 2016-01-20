%% Feel free to use, reuse and abuse the code in this file.

-module(pubsub).

-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/0, stop/1]).

-define(TOPIC, <<"PUBSUB_TOPIC">>).

%% API.

start() ->
  application:ensure_all_started(pubsub).

stop() ->
  application:stop(pubsub).

start(_Type, _Args) ->
  P = spawn_link(fun() -> publisher(?TOPIC) end),
  lists:foreach(fun(N) -> subscriber(?TOPIC, N) end, lists:seq(1, 3)),
  timer:sleep(1 * 60 * 1000),
  exit(P, kill),
  teardown_ebus().

stop(_State) ->
  ok.

%% Internals

publisher(Topic) ->
  timer:sleep(500),
  Msg = #{topic => Topic, payload => os:timestamp()},
  io:format("Publish: ~p~n", [Msg]),
  ebus:pub(Topic, Msg),
  publisher(Topic).

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
