-module(chat_erlbus_handler).

%% API
-export([handle_msg/2]).

handle_msg(Msg, Context) ->
  Context ! {message_published, Msg}.
  