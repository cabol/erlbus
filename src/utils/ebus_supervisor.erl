%%%-------------------------------------------------------------------
%%% @doc
%%% Utility module to start/work with supervisors.
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/2, start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% The Supervisor name
-type name() :: atom() | {global, term()} | {via, module(), term()}.

%% Options used by the `start*` functions
-type option() :: {name, name()} |
                  {strategy, supervisor:strategy()} |
                  {max_restarts, non_neg_integer()} |
                  {max_seconds, non_neg_integer()}.

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(
  [ebus_supervisor_spec:spec()], [option()]
) -> supervisor:startlink_ret().
start_link(Children, Options) when is_list(Children) ->
  Spec = ebus_supervisor_spec:supervise(Children, Options),
  start_link(?MODULE, Spec, Options).

-spec start_link(module(), term(), [option()]) -> supervisor:startlink_ret().
start_link(Module, Arg, Options) ->
  case ebus_utils:keyfind(name, Options, nil) of
    nil ->
      supervisor:start_link(Module, Arg);
    Atom when is_atom(Atom) ->
      supervisor:start_link({local, Atom}, Module, Arg);
    Other when is_tuple(Other) ->
      supervisor:start_link(Other, Module, Arg)
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init(Args) -> Args.
