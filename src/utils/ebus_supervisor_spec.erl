%%%-------------------------------------------------------------------
%%% @doc
%%% Utility module to create supervisor and worker specs.
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_supervisor_spec).

%% API
-export([supervise/2]).
-export([supervisor/2, supervisor/3]).
-export([worker/2, worker/3]).

%% Supervisor options
-type sup_option() :: {strategy, supervisor:strategy()} |
                      {max_restarts, non_neg_integer()} |
                      {max_seconds, non_neg_integer()}.

%% Child Spec options
-type spec_option() :: {restart, supervisor:restart()} |
                       {shutdown, supervisor:shutdown()} |
                       {id, term()} |
                       {function, atom()} |
                       {modules, dynamic | [module()]}.

%% The supervisor specification
-type spec() :: supervisor:child_spec().

-export_type([sup_option/0, spec_option/0, spec/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec supervise([spec()], [sup_option()]) -> {ok, tuple()}.
supervise(Children, Opts) ->
  Strategy = case ebus_utils:keyfind(strategy, Opts, nil) of
               nil -> throw({missing_option, strategy});
               Val -> Val
             end,

  MaxR = ebus_utils:keyfind(max_restarts, Opts, 3),
  MaxS = ebus_utils:keyfind(max_seconds, Opts, 5),

  assert_unique_ids([element(1, S) || S <- Children]),
  {ok, {{Strategy, MaxR, MaxS}, Children}}.

%% @private
assert_unique_ids([]) ->
  ok;
assert_unique_ids([Id | Rest]) ->
  case lists:member(Id, Rest) of
    true -> throw({badarg, duplicated_id});
    _    -> assert_unique_ids(Rest)
  end.

%% @equiv supervisor(Module, Args, [])
supervisor(Module, Args) ->
  supervisor(Module, Args, []).

-spec supervisor(module(), [term()], [spec_option()]) -> spec().
supervisor(Module, Args, Opts) ->
  child(supervisor, Module, Args, [{shutdown, infinity} | Opts]).

%% @equiv worker(Module, Args, [])
worker(Module, Args) ->
  worker(Module, Args, []).

-spec worker(module(), [term()], [spec_option()]) -> spec().
worker(Module, Args, Opts) ->
  child(worker, Module, Args, Opts).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
child(Type, Module, Args, Opts) ->
  Id       = ebus_utils:keyfind(id, Opts, Module),
  Modules  = ebus_utils:keyfind(modules, Opts, [Module]),
  Function = ebus_utils:keyfind(function, Opts, start_link),
  Restart  = ebus_utils:keyfind(restart, Opts, permanent),
  Shutdown = ebus_utils:keyfind(shutdown, Opts, 5000),

  {Id, {Module, Function, Args}, Restart, Shutdown, Type, Modules}.
