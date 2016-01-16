-module(ebus_broadcast).

%% API
-export([new/0, new/1, new/2, new/3]).
-export([from_map/1]).

%%%===================================================================
%%% Type definitions
%%%===================================================================

%% @type broadcast() =
%% #{
%%   topic   => binary() | nil,
%%   event   => binary() | nil,
%%   payload => term(),
%%   ebus_t  => broadcast
%% }.
%%
%% Defines a message sent from pubsub to channels and vice-versa.
%% The message format requires the following keys:
%% <ul>
%% <li>`topic': The binary topic or `topic:subtopic` pair namespace,
%% for example `<<"messages">>', `<<"messages:123">>'.</li>
%% <li>`event': The binary event name, for example `<<"ebus_join">>'.</li>
%% <li>`payload': The message payload.</li>
%% </ul>
-type broadcast() :: #{
  topic   => binary() | nil,
  event   => binary() | nil,
  payload => term(),
  ebus_t  => broadcast
}.

-export_type([broadcast/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv new(nil)
new() ->
  new(nil).

%% @equiv new(Topic, nil)
new(Topic) ->
  new(Topic, nil).

%% @equiv new(Topic, Event, nil)
new(Topic, Event) ->
  new(Topic, Event, nil).

-spec new(binary() | nil, binary() | nil, term()) -> broadcast().
new(Topic, Event, Payload)
    when (is_binary(Topic) orelse Topic =:= nil)
    andalso (is_binary(Event) orelse Event =:= nil) ->
  #{
    topic   => Topic,
    event   => Event,
    payload => Payload,
    ebus_t  => broadcast
  }.

-spec from_map(map()) -> broadcast().
from_map(Map) ->
  BinKey = fun(K, V, Acc) -> Acc#{ebus_common:to_bin(K) => V} end,
  BinMap = maps:fold(BinKey, #{}, Map),
  #{
    topic   => maps:get(<<"topic">>, BinMap, nil),
    event   => maps:get(<<"event">>, BinMap, nil),
    payload => maps:get(<<"payload">>, BinMap, nil),
    ebus_t  => broadcast
  }.
