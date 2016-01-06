-module(ebus_message).

%% API
-export([from_map/1, to_map/1]).

-include("ebus.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec from_map(map()) -> message().
from_map(Map) ->
  F = ?proplist_to_record(message, #message{}),
  F(maps:to_list(Map)).

-spec to_map(message()) -> map().
to_map(Message) ->
  F = ?record_to_proplist(message),
  maps:from_list(F(Message)).
