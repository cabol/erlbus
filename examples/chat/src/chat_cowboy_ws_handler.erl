-module(chat_cowboy_ws_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(CHATROOM_NAME, ?MODULE).
-define(TIMEOUT, 5 * 60 * 1000). % Innactivity Timeout

-record(state, {name, handler}).

%% API

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
  % Create the handler from our custom handler
  Handler = ebus_handler:new(chat_erlbus_handler, self()),
  ebus:sub(?CHATROOM_NAME, Handler),
  {ok, Req, #state{name = get_name(Req), handler = Handler}, ?TIMEOUT}.

websocket_handle({text, Msg}, Req, State) ->
  ebus:pub(?CHATROOM_NAME, {State#state.name, Msg}),
  {ok, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({message_published, {Sender, Msg}}, Req, State) ->
  {reply, {text, jiffy:encode({[{sender, Sender}, {msg, Msg}]})}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
  % Unsubscribe the handler
  ebus:unsub(?CHATROOM_NAME, State#state.handler),
  ok.

%% Private methods

get_name(Req) ->
  {{Host, Port}, _} = cowboy_req:peer(Req),
  Name = list_to_binary(string:join([inet_parse:ntoa(Host), 
    ":", io_lib:format("~p", [Port])], "")),
  Name.
  