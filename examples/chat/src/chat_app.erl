-module(chat_app).
-behaviour(application).

-export([start/0, start/2]).
-export([stop/1]).

start() ->
  application:ensure_all_started(chat).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
  {'_', [
  {"/", cowboy_static, {priv_file, chat, "index.html"}}
  ,
  {"/websocket", chat_cowboy_ws_handler, []},
  {"/assets/[...]", cowboy_static, {priv_dir, chat, "assets"}}
  ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
  [{env, [{dispatch, Dispatch}]}]),
  chat_sup:start_link().

stop(_State) ->
  ok.
