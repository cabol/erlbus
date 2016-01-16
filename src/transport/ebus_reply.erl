-module(ebus_reply).

%% API
-export([new/0, new/1, new/2, new/3, new/4]).
-export([from_map/1]).

%%%===================================================================
%%% Type definitions
%%%===================================================================

%% @type reply() =
%% #{
%%   topic   => binary() | nil,
%%   status  => atom(),
%%   payload => term(),
%%   ref     => binary() | nil,
%%   ebus_t  => reply
%% }.
%%
%% Defines a reply sent from channels to transports.
%% The message format requires the following keys:
%% <ul>
%% <li>`topic': The binary topic or `topic:subtopic` pair namespace,
%% for example `<<"messages">>', `<<"messages:123">>'.</li>
%% <li>`status': The reply status as an atom.</li>
%% <li>`payload': The message payload.</li>
%% <li>`ref': The unique binary ref.</li>
%% </ul>
-type reply() :: #{
  topic   => binary() | nil,
  status  => atom(),
  payload => term(),
  ref     => binary() | nil,
  ebus_t  => reply
}.

-export_type([reply/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv new(nil)
new() ->
  new(nil).

%% @equiv new(Topic, nil)
new(Topic) ->
  new(Topic, nil).

%% @equiv new(Topic, Status, nil)
new(Topic, Status) ->
  new(Topic, Status, nil).

%% @equiv new(Topic, Status, Payload, nil)
new(Topic, Status, Payload) ->
  new(Topic, Status, Payload, nil).

-spec new(binary() | nil, atom(), term(), binary() | nil) -> reply().
new(Topic, Status, Payload, Ref)
    when (is_binary(Topic) orelse Topic =:= nil) andalso is_atom(Status) ->
  #{
    topic   => Topic,
    status  => Status,
    payload => Payload,
    ref     => Ref,
    ebus_t  => reply
  }.

-spec from_map(map()) -> reply().
from_map(Map) ->
  BinKey = fun(K, V, Acc) -> Acc#{ebus_common:to_bin(K) => V} end,
  BinMap = maps:fold(BinKey, #{}, Map),
  #{
    topic   => maps:get(<<"topic">>, BinMap, nil),
    status  => ebus_common:to_atom(maps:get(<<"status">>, BinMap, nil)),
    payload => maps:get(<<"payload">>, BinMap, nil),
    ref     => maps:get(<<"ref">>, BinMap, nil),
    ebus_t  => reply
  }.
