%%%-------------------------------------------------------------------
%%% @doc
%%% Main entry point for `ebus` functions.
%%% This is also a wrapper for `ebus_ps` module.
%%% @end
%%%-------------------------------------------------------------------
-module(ebus).

-behaviour(application).

%% PubSub API
-export([sub/2, sub/3, sub/4]).
-export([unsub/2, unsub/3]).
-export([pub/2, pub/3, pub_from/3, pub_from/4]).
-export([subscribers/1, subscribers/2]).
-export([local_subscribers/1, local_subscribers/2]).
-export([topics/0, topics/1, local_topics/0, local_topics/1]).
-export([dispatch/2, dispatch/3, dispatch/4]).

%% Application callbacks and functions
-export([start/0, stop/0]).
-export([start/2, stop/1]).

%% Utilities
-export([default_ps_server/0]).

%%%===================================================================
%%% Types
%%%===================================================================

% Dispatch options
-type dispatch_fun()  :: fun(([term()]) -> term()).
-type dispatch_opt()  :: {scope, local | global} |
                         {dispatch_fun, dispatch_fun()}.
-type dispatch_opts() :: [dispatch_opt()].

-export_type([dispatch_opts/0]).

%%%===================================================================
%%% PubSub API
%%%===================================================================

%% @equiv sub(server(), Handler, Topic)
sub(Handler, Topic) ->
  sub(server(), Handler, Topic).

%% @equiv sub(Server, Handler, Topic, [])
sub(Server, Handler, Topic) ->
  sub(Server, Handler, Topic, []).

-spec sub(atom(), pid(), iodata(), [term()]) -> ok | {error, term()}.
sub(Server, Handler, Topic, Opts) ->
  ebus_ps:subscribe(Server, Handler, ebus_utils:to_bin(Topic), Opts).

%% @equiv unsub(server(), Handler, Topic)
unsub(Handler, Topic) ->
  unsub(server(), Handler, Topic).

-spec sub(atom(), pid(), iodata()) -> ok | {error, term()}.
unsub(Server, Handler, Topic) ->
  ebus_ps:unsubscribe(Server, Handler, ebus_utils:to_bin(Topic)).

%% @equiv pub(server(), Topic, Message)
pub(Topic, Message) ->
  pub(server(), Topic, Message).

-spec pub(atom(), iodata(), term()) -> ok | {error, term()}.
pub(Server, Topic, Message) ->
  ebus_ps:broadcast(Server, ebus_utils:to_bin(Topic), Message).

%% @equiv pub_from(server(), From, Topic, Message)
pub_from(From, Topic, Message) ->
  pub_from(server(), From, Topic, Message).

-spec pub_from(atom(), pid(), iodata(), term()) -> ok | {error, term()}.
pub_from(Server, From, Topic, Message) ->
  ebus_ps:broadcast_from(Server, From, ebus_utils:to_bin(Topic), Message).

%% @equiv subscribers(server(), Topic)
subscribers(Topic) ->
  subscribers(server(), Topic).

-spec subscribers(atom(), iodata()) -> [pid()].
subscribers(Server, Topic) ->
  Nodes = nodes(),
  BinTopic = ebus_utils:to_bin(Topic),
  RemoteSubscribers = lists:foldl(
    fun(Node, Acc) ->
      Acc ++ rpc:call(Node, ?MODULE, local_subscribers, [Server, BinTopic])
    end, [], Nodes
  ),
  RemoteSubscribers ++ local_subscribers(Server, BinTopic).

%% @equiv local_subscribers(server(), Topic)
local_subscribers(Topic) ->
  local_subscribers(server(), Topic).

-spec local_subscribers(atom(), iodata()) -> [pid()].
local_subscribers(Server, Topic) ->
  ebus_ps:subscribers(Server, ebus_utils:to_bin(Topic)).

%% @equiv topics(server())
topics() ->
  topics(server()).

-spec topics(atom()) -> [binary()].
topics(Server) ->
  Nodes = nodes(),
  RemoteTopics = lists:foldl(
    fun(Node, Acc) ->
      Acc ++ rpc:call(Node, ?MODULE, local_topics, [Server])
    end, [], Nodes
  ),
  lists:usort(RemoteTopics ++ local_topics(Server)).

%% @equiv local_topics(server())
local_topics() ->
  local_topics(server()).

-spec local_topics(atom()) -> [binary()].
local_topics(Server) ->
  ebus_ps:list(Server).

%% @equiv dispatch(Topic, Message, [])
dispatch(Topic, Message) ->
  dispatch(Topic, Message, []).

%% @equiv dispatch(server(), Topic, Message, Opts)
dispatch(Topic, Message, Opts) ->
  dispatch(server(), Topic, Message, Opts).

-spec dispatch(atom(), iodata(), term(), dispatch_opts()) -> ok.
dispatch(Server, Topic, Message, Opts) ->
  BinTopic = ebus_utils:to_bin(Topic),
  Subscribers = case ebus_utils:keyfind(scope, Opts, local) of
    local -> local_subscribers(Server, BinTopic);
    _     -> subscribers(Server, BinTopic)
  end,
  DispatchFun = case ebus_utils:keyfind(dispatch_fun, Opts, nil) of
    nil -> fun ebus_utils:rand_elem/1;
    Fun -> Fun
  end,
  case Subscribers of
    [] -> throw(no_subscribers_available);
    _  -> DispatchFun(Subscribers) ! Message, ok
  end.

%%%===================================================================
%%% Application callbacks and functions
%%%===================================================================

-spec start() -> {ok, _} | {error, term()}.
start() -> application:ensure_all_started(ebus).

-spec stop() -> ok | {error, term()}.
stop() -> application:stop(ebus).

%% @hidden
start(_StartType, _StartArgs) -> ebus_sup:start_link().

%% @hidden
stop(_State) -> ok.

%%%===================================================================
%%% Utilities
%%%===================================================================

-spec default_ps_server() -> ebus_ps.
default_ps_server() -> ebus_ps.

%%%===================================================================
%%% Internal function
%%%===================================================================

%% @private
server() ->
  PubSub = application:get_env(ebus, pubsub, []),
  ebus_utils:keyfind(name, PubSub, default_ps_server()).
