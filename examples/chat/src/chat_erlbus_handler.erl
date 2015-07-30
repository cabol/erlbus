-module(chat_erlbus_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({_Channel, Msg}, Context) ->
  Context ! {message_published, Msg}.
  