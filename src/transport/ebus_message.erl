-module(ebus_message).

%% API
-export([new/0, new/1, new/2, new/3, new/4]).
-export([from_map/1]).

%%%===================================================================
%%% Type definitions
%%%===================================================================

%% @type message() =
%% #{
%%   topic   => binary() | nil,
%%   event   => binary() | nil,
%%   payload => term(),
%%   ref     => binary() | nil,
%%   ebus_t  => message
%% }.
%%
%% Defines a message dispatched over transport to channels and vice-versa.
%% The message format requires the following keys:
%% <ul>
%% <li>`topic': The binary topic or `topic:subtopic` pair namespace,
%% for example `<<"messages">>', `<<"messages:123">>'.</li>
%% <li>`event': The binary event name, for example `<<"ebus_join">>'.</li>
%% <li>`payload': The message payload.</li>
%% <li>`ref': The unique binary ref.</li>
%% </ul>
-type message() :: #{
  topic   => binary() | nil,
  event   => binary() | nil,
  payload => term(),
  ref     => binary() | nil,
  ebus_t  => message
}.

-export_type([message/0]).

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

%% @equiv new(Topic, Event, Payload, nil)
new(Topic, Event, Payload) ->
  new(Topic, Event, Payload, nil).

-spec new(binary() | nil, binary() | nil, term(), binary() | nil) -> message().
new(Topic, Event, Payload, Ref)
    when (is_binary(Topic) orelse Topic =:= nil)
    andalso (is_binary(Event) orelse Event =:= nil) ->
  #{
    topic   => Topic,
    event   => Event,
    payload => Payload,
    ref     => Ref,
    ebus_t  => message
  }.

-spec from_map(map()) -> message().
from_map(Map) ->
  BinKey = fun(K, V, Acc) -> Acc#{ebus_common:to_bin(K) => V} end,
  BinMap = maps:fold(BinKey, #{}, Map),
  #{
    topic   => maps:get(<<"topic">>, BinMap, nil),
    event   => maps:get(<<"event">>, BinMap, nil),
    payload => maps:get(<<"payload">>, BinMap, nil),
    ref     => maps:get(<<"ref">>, BinMap, nil),
    ebus_t  => message
  }.
