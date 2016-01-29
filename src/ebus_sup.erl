%%%-------------------------------------------------------------------
%%% @doc
%%% Starts main supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_sup).

%% API
-export([start_link/0]).

-define(SUPERVISOR, 'Elixir.Supervisor').
-define(SUPERVISOR_SPEC, 'Elixir.Supervisor.Spec').
-define(PHX_PUBSUB_PG2, 'Elixir.Phoenix.PubSub.PG2').

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  PubSub = application:get_env(ebus, pubsub, []),
  Name = ebus_common:keyfind(name, PubSub, ebus:default_ps_server()),
  Adapter = ebus_common:keyfind(adapter, PubSub, ?PHX_PUBSUB_PG2),
  Opts = ebus_common:keyfind(opts, PubSub, [{pool_size, 1}]),
  Children = [?SUPERVISOR_SPEC:supervisor(Adapter, [Name, Opts])],
  ?SUPERVISOR:start_link(Children, [{strategy, one_for_one}]).
