%%%-------------------------------------------------------------------
%%% @doc
%%% Starts main supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_sup).

%% API
-export([start_link/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  PubSub = application:get_env(ebus, pubsub, []),
  Name = ebus_common:keyfind(name, PubSub, ebus:default_ps_server()),
  Adapter = ebus_common:keyfind(adapter, PubSub, ebus_ps_pg2),
  Adapter:start_link(Name, PubSub).
