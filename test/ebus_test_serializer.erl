-module(ebus_test_serializer).

-behaviour(ebus_serializer).

%% API
-export([fastlane/1, encode/1, decode/2]).

-include_lib("ebus/include/ebus.hrl").

fastlane(#broadcast{topic = Topic, event = Event, payload = PL} = Msg) ->
  whereis(ebus_ps_test_subscriber) ! {fastlaned, Msg},
  #message{topic = Topic, event = Event, payload = PL}.

encode(#reply{topic = Topic, status = Status, payload = PL, ref = Ref}) ->
  #message{
    topic = Topic,
    event = <<"ebus_reply">>,
    ref = Ref,
    payload = #{status => Status, response => PL}
  };
encode(#message{} = Msg) ->
  Msg.

decode(Message, _Opts) -> Message.
