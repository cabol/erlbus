%% Feel free to use, reuse and abuse the code in this file.

-module(pub_sub).

-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/0, stop/1]).

-define(HANDLER, pub_sub_handler).
-define(TOPIC, pub_sub_topic).

%% API.

start() ->
  application:ensure_all_started(pub_sub).

start(_Type, _Args) ->
  P = spawn_link(fun() -> publisher(?TOPIC) end),
  lists:foreach(fun(N) -> subscriber(?TOPIC, N) end, lists:seq(1, 3)),
  timer:sleep(1 * 60 * 1000),
  exit(P, kill),
  teardown_ebus().

stop() ->
  application:stop(pub_sub).

stop(_State) ->
  ok.

%% Internals

publisher(Topic) ->
  timer:sleep(500),
  Payload = now(),
  io:format("Publish: ~p~n", [Payload]),
  ebus:pub(Topic, Payload),
  publisher(Topic).

subscriber(Topic, N) ->
  Handler = ebus_handler:new(?HANDLER, N),
  ebus:sub(Topic, Handler).

teardown_ebus() ->
  application:stop(ebus).
