-module(ebus_test_serializer).

-behaviour(ebus_serializer).

%% API
-export([fastlane/1, encode/1, decode/2]).

fastlane(#{topic := Topic,
           event := Event,
           payload := PL,
           ebus_t := broadcast} = Msg) ->
  whereis(ebus_ps_test_subscriber) ! {fastlaned, Msg},
  ebus_message:new(Topic, Event, PL).

encode(#{topic := Topic,
         status := Status,
         payload := PL,
         ref := Ref,
         ebus_t := reply}) ->
  ebus_message:new(
    Topic,
    <<"ebus_reply">>,
    #{status => Status, response => PL},
    Ref
  );
encode(#{ebus_t := message} = Msg) ->
  Msg.

decode(Message, _Opts) -> Message.
