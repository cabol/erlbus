-module(ebus_reply).

%% API
-export([from_map/1, to_map/1]).

-include("ebus.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec from_map(map()) -> reply().
from_map(Map) ->
  F = ?proplist_to_record(reply, #reply{}),
  F(maps:to_list(Map)).

-spec to_map(reply()) -> map().
to_map(Message) ->
  F = ?record_to_proplist(reply),
  maps:from_list(F(Message)).
