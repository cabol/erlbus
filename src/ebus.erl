%%%-------------------------------------------------------------------
%%% @doc
%%% Main `ebus` interface. This module also works as a wrapper on top
%%% of `ebus_ps' module.
%%% @see ebus_ps
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
-export([server/0, default_ps_server/0]).

%%%===================================================================
%%% Types
%%%===================================================================

%% @type topic() = iodata().
-type topic() :: iodata().

%% @type handler() = pid().
-type handler() :: pid().

%% @type dispatch_fun() = fun(([term()]) -> term()).
%%
%% Receives as argument the subscribers list. Then it should choose
%% one subscriber from the given list and return it.
-type dispatch_fun() :: fun(([term()]) -> term()).

%% @type dispatch_opt() =
%% {scope, local | global} | {dispatch_fun, dispatch_fun()}.
%%
%% Available dispatch options.
-type dispatch_opt() :: {scope, local | global} |
                        {dispatch_fun, dispatch_fun()}.

%% @type dispatch_opts() = [dispatch_opt()].
%%
% Dispatch options.
-type dispatch_opts() :: [dispatch_opt()].

%% @type options() = ebus_ps_local:options().
%%
%% `sub/4' options.
-type options() :: ebus_ps_local:options().

% Exported types
-export_type([
  topic/0,
  handler/0,
  dispatch_fun/0,
  dispatch_opts/0,
  options/0
]).

%%%===================================================================
%%% PubSub API
%%%===================================================================

%% @equiv sub(server(), Handler, Topic)
sub(Handler, Topic) ->
  sub(server(), Handler, Topic).

%% @equiv sub(Server, Handler, Topic, [])
sub(Server, Handler, Topic) ->
  sub(Server, Handler, Topic, []).

%% @doc
%% Subscribes the `Handler' given `Topic'.
%%
%% <ul>
%% <li>`Server': The Pid registered name of the server.</li>
%% <li>`Handler': The subscriber pid to receive pubsub messages.</li>
%% <li>`Topic': The topic to subscribe to, ie: `"users:123"'.</li>
%% <li>`Opts': The optional list of options. See below.</li>
%% </ul>
%%
%% <b>Options:</b>
%% <br/>
%% <ul>
%% <li>`{link, _}': links the subscriber to the pubsub adapter.</li>
%% <li>`{fastlane, ebus_ps_local:fastlane()}': Provides a fastlane path
%% for the broadcasts for `broadcast()' events. The fastlane process is
%% notified of a cached message instead of the normal subscriber.
%% Fastlane handlers must implement `fastlane/1' callbacks which accepts a
%% `broadcast()' struct and returns a fastlaned format for the handler.</li>
%% </ul>
%%
%% Examples:
%%
%% ```
%% > ebus:sub(self(), <<"foo">>).
%% ok
%% > ebus:sub(ebus_ps, self(), <<"foo">>).
%% ok
%% > ebus:sub(ebus_ps, self(), <<"foo">>, []).
%% ok
%% > ebus:sub(ebus_ps, self(), <<"foo">>,
%%     [{fastlane, {FastPid, my_serializer, [<<"event1">>]}]).
%% ok
%% '''
%% @end
-spec sub(atom(), handler(), topic(), options()) -> ok | {error, term()}.
sub(Server, Handler, Topic, Opts) ->
  ebus_ps:subscribe(Server, Handler, ebus_common:to_bin(Topic), Opts).

%% @equiv unsub(server(), Handler, Topic)
unsub(Handler, Topic) ->
  unsub(server(), Handler, Topic).

%% @doc
%% Unsubscribes the given `Handler' from the `Topic'.
%%
%% <ul>
%% <li>`Server': The registered server name or pid.</li>
%% <li>`Handler': The subscriber pid.</li>
%% <li>`Topic': The string topic, for example `<<"users:123">>'.</li>
%% </ul>
%%
%% Example:
%%
%% ```
%% > ebus:unsub(self(), <<"foo">>).
%% ok
%% > ebus:unsub(ebus_ps, self(), <<"foo">>).
%% ok
%% '''
%% @end
-spec unsub(atom(), handler(), topic()) -> ok | {error, term()}.
unsub(Server, Handler, Topic) ->
  ebus_ps:unsubscribe(Server, Handler, ebus_common:to_bin(Topic)).

%% @equiv pub(server(), Topic, Message)
pub(Topic, Message) ->
  pub(server(), Topic, Message).

%% @doc
%% Sends a message to all subscribers of a topic.
%%
%% <ul>
%% <li>`Server': The registered server name or pid.</li>
%% <li>`Topic': The string topic, for example `<<"users:123">>'.</li>
%% <li>`Message': Any erlang term.</li>
%% </ul>
%%
%% Examples:
%%
%% ```
%% > ebus:pub("bar", #{topic => "foo", payload => "hi"}).
%% ok
%% > ebus:pub(ebus_ps, "bar", #{topic => "foo", payload => "hi"}).
%% ok
%% '''
%% @end
-spec pub(atom(), topic(), term()) -> ok | {error, term()}.
pub(Server, Topic, Message) ->
  ebus_ps:broadcast(Server, ebus_common:to_bin(Topic), Message).

%% @equiv pub_from(server(), From, Topic, Message)
pub_from(From, Topic, Message) ->
  pub_from(server(), From, Topic, Message).

%% @doc
%% Same as `pub/3' but message is not sent to `FromHandler'.
%%
%% Examples:
%%
%% ```
%% > ebus:pub_from(self(), "foo", <<"message">>).
%% ok
%% > ebus:pub_from(ebus_ps, self(), "foo", <<"message">>).
%% ok
%% '''
%% @end
-spec pub_from(atom(), handler(), topic(), term()) -> ok | {error, term()}.
pub_from(Server, FromHandler, Topic, Message) ->
  BinTopic = ebus_common:to_bin(Topic),
  ebus_ps:broadcast_from(Server, FromHandler, BinTopic, Message).

%% @equiv subscribers(server(), Topic)
subscribers(Topic) ->
  subscribers(server(), Topic).

%% @doc
%% Returns a set of all subscribers handlers (local and global)
%% for the given `Topic'.
%%
%% <ul>
%% <li>`Server': The registered server name or pid.</li>
%% <li>`Topic': The string topic, for example `<<"users:123">>'.</li>
%% </ul>
%%
%% Example:
%%
%% ```
%% > ebus:subscribers(ebus_ps, <<"foo">>).
%% [<0.48.0>, <0.49.0>]
%% '''
%% @end
-spec subscribers(atom(), topic()) -> [pid()].
subscribers(Server, Topic) ->
  BinTopic = ebus_common:to_bin(Topic),
  {ResL, _} = rpc:multicall(?MODULE, local_subscribers, [Server, BinTopic]),
  lists:merge(ResL).

%% @equiv local_subscribers(server(), Topic)
local_subscribers(Topic) ->
  local_subscribers(server(), Topic).

%% @doc
%% Same as `subscribers/2' but only local subscribers handlers for the
%% given `Topic' are returned.
%%
%% Example:
%%
%% ```
%% > ebus:local_subscribers(ebus_ps, <<"foo">>).
%% [<0.48.0>, <0.49.0>]
%% '''
%% @end
-spec local_subscribers(atom(), topic()) -> [pid()].
local_subscribers(Server, Topic) ->
  ebus_ps:subscribers(Server, ebus_common:to_bin(Topic)).

%% @equiv topics(server())
topics() ->
  topics(server()).

%% @doc
%% Returns the list of all topics (local and global) in use.
%% This is an expensive and private operation.
%%
%% <p><font color="red">This is an expensive operation.
%% <b> DO NOT USE IT IN PROD</b></font></p>
%%
%% Example:
%%
%% ```
%% > ebus:topics().
%% [<<"foo">>, <<"bar">>]
%% > ebus:topics(ebus_ps).
%% [<<"foo">>, <<"bar">>]
%% '''
%% @end
-spec topics(atom()) -> [binary()].
topics(Server) ->
  {ResL, _} = rpc:multicall(?MODULE, local_topics, [Server]),
  lists:usort(lists:merge(ResL)).

%% @equiv local_topics(server())
local_topics() ->
  local_topics(server()).

%% @doc
%% Same as `topics/1' but only local topics are returned.
%%
%% Example:
%%
%% ```
%% > ebus:local_topics().
%% [<<"foo">>, <<"bar">>]
%% > ebus:local_topics(ebus_ps).
%% [<<"foo">>, <<"bar">>]
%% '''
%% @end
-spec local_topics(atom()) -> [binary()].
local_topics(Server) ->
  ebus_ps:list(Server).

%% @equiv dispatch(Topic, Message, [])
dispatch(Topic, Message) ->
  dispatch(Topic, Message, []).

%% @equiv dispatch(server(), Topic, Message, Opts)
dispatch(Topic, Message, Opts) ->
  dispatch(server(), Topic, Message, Opts).

%% @doc
%% Sends a message only to one subscriber handler of the `Topic'.
%%
%% <ul>
%% <li>`Server': The registered server name or pid.</li>
%% <li>`Topic': The string topic, for example `<<"users:123">>'.</li>
%% <li>`Message': Any erlang term.</li>
%% <li>`Opts': The optional list of options. See below.</li>
%% </ul>
%%
%% <b>Options:</b>
%% <br/>
%% <ul>
%% <li>`{scope, local | global}': define if the message must be delivered
%% to a local or global (any) process. Default is `local'.</li>
%% <li>`{dispatch_fun, dispatch_fun()}': allows to pass a function to
%% choose a subscriber from the current subscribers handlers to a topic.</li>
%% </ul>
%%
%% Examples:
%%
%% ```
%% > ebus:dispatch("bar", #{topic => "foo", payload => "hi"}).
%% ok
%% > ebus:dispatch("bar", #{topic => "foo", payload => "hi"}, []).
%% ok
%% > ebus:dispatch(ebus_ps, "bar", "my message",
%%     [{scope, global}, {dispatch_fun, fun([H | _]) -> H end}]).
%% ok
%% '''
%% @end
-spec dispatch(atom(), topic(), term(), dispatch_opts()) -> ok.
dispatch(Server, Topic, Message, Opts) ->
  BinTopic = ebus_common:to_bin(Topic),
  Subscribers = case ebus_common:keyfind(scope, Opts, local) of
    local -> local_subscribers(Server, BinTopic);
    _     -> subscribers(Server, BinTopic)
  end,
  DispatchFun = case ebus_common:keyfind(dispatch_fun, Opts) of
    nil -> fun ebus_common:rand_elem/1;
    Fun -> Fun
  end,
  case Subscribers of
    [] -> throw(no_subscribers_available);
    _  -> DispatchFun(Subscribers) ! Message, ok
  end.

%%%===================================================================
%%% Application callbacks and functions
%%%===================================================================

%% @doc Starts `ebus' application.
-spec start() -> {ok, _} | {error, term()}.
start() -> application:ensure_all_started(ebus).

%% @doc Stops `ebus' application.
-spec stop() -> ok | {error, term()}.
stop() -> application:stop(ebus).

%% @hidden
start(_StartType, _StartArgs) -> ebus_sup:start_link().

%% @hidden
stop(_State) -> ok.

%%%===================================================================
%%% Utilities
%%%===================================================================

%% @doc Returns the registered `ebus' server name.
-spec server() -> atom().
server() ->
  PubSub = application:get_env(ebus, pubsub, []),
  ebus_common:keyfind(name, PubSub, default_ps_server()).

%% @doc Returns default `ebus' server name: `ebus_ps'.
-spec default_ps_server() -> ebus_ps.
default_ps_server() -> ebus_ps.
