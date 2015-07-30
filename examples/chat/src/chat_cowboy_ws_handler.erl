-module(chat_cowboy_ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-define(CHATROOM_NAME, ?MODULE).
-define(TIMEOUT, 5 * 60 * 1000). % Innactivity Timeout

-record(state, {name}).

%% API

init(Req, _Opts) ->
  % Create the handler from our custom handler
  Handler = ebus_handler:new(chat_erlbus_handler, self()),
  ebus:sub(?CHATROOM_NAME, Handler),
  {cowboy_websocket, Req, #state{name=get_name(Req)}, ?TIMEOUT}.

websocket_handle({text, Msg}, Req, State) ->
  ebus:pub(?CHATROOM_NAME, {State#state.name, Msg}),
  {ok, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({message_published, {Sender, Msg}}, Req, State) ->
  {reply, {text, jiffy:encode({[{sender, Sender}, {msg, Msg}]})}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%% Private methods

get_name(Req) ->
  {Host, Port} = cowboy_req:peer(Req),
  Name = list_to_binary(string:join([inet_parse:ntoa(Host), 
    ":", io_lib:format("~p", [Port])], "")),
  Name.