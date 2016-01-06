-module(ebus_broadcast).

%% API
-export([from_map/1, to_map/1]).

-include("ebus.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec from_map(map()) -> broadcast().
from_map(Map) ->
  F = ?proplist_to_record(broadcast, #broadcast{}),
  F(maps:to_list(Map)).

-spec to_map(broadcast()) -> map().
to_map(Message) ->
  F = ?record_to_proplist(broadcast),
  maps:from_list(F(Message)).
