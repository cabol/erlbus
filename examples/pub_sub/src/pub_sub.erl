%% Feel free to use, reuse and abuse the code in this file.

-module(pub_sub).

-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/0, stop/1]).

-define(HANDLER, pub_sub_handler).
-define(CHANNEL, pub_sub_channel).

%% API.

start() ->
  application:ensure_all_started(pub_sub).

start(_Type, _Args) ->
  P = spawn_link(fun() -> publisher(?CHANNEL) end),
  lists:foreach(fun(N) -> subscriber(?CHANNEL, N) end, lists:seq(1, 3)),
  timer:sleep(1 * 60 * 1000),
  exit(P, kill),
  teardown_ebus().

stop() ->
  application:stop(pub_sub).

stop(_State) ->
  ok.

%% Internals

publisher(Channel) ->
  timer:sleep(500),
  Payload = now(),
  io:format("Publish: ~p~n", [Payload]),
  ebus:pub(Channel, Payload),
  publisher(Channel).

subscriber(Channel, N) ->
  Handler = ebus_handler:new(?HANDLER, N),
  ebus:sub(Channel, Handler).

teardown_ebus() ->
  application:stop(ebus).
