%%%-------------------------------------------------------------------
%%% @doc
%%% This is an Erlang clone of the original `Phoenix.PubSub.Local`
%%% module.
%%% Copyright (c) 2014 Chris McCord
%%% @see <a href="https://github.com/phoenixframework/phoenix"></a>
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_ps_local).

-behaviour(gen_server).

%% API
-export([
  start_link/2,
  subscribe/4,
  subscribe/5,
  unsubscribe/4,
  broadcast/5,
  subscribers/3,
  subscribers_by_shard/3,
  subscribers_with_fastlanes/3,
  list/2,
  list_by_shard/2,
  subscription/3,
  local_name/2,
  gc_name/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("ebus.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server.
%%
%% * `ServerName`: The name to register the server under
%% @end
-spec start_link(atom(), atom()) -> gen:start_ret().
start_link(ServerName, GCName) ->
  gen_server:start_link(
    {local, ServerName}, ?MODULE, [ServerName, GCName], []
  ).

%% @equiv subscribe(Server, PoolSize, Pid, Topic, [])
subscribe(Server, PoolSize, Pid, Topic) ->
  subscribe(Server, PoolSize, Pid, Topic, []).

%% @doc
%% Subscribes the pid to the topic.
%%
%% * `Server`: The registered server name or pid
%% * `Pid`: The subscriber pid
%% * `Topic`: The string topic, for example <<"users:123">>
%% * `Opts`: The optional list of options.
%%
%% Options:
%%
%% * `link`: links the subscriber to local
%% * `fastlane`: Provides a fastlane path for the broadcasts for
%% `ebus_broadcast:broadcast()` events. The fastlane process is
%% notified of a cached message instead of the normal subscriber.
%% Fastlane handlers must implement `fastlane/1` callbacks which
%% accepts a `ebus_broadcast:broadcast()` type and returns a
%% fastlaned format for the handler.
%%
%% Examples:
%%
%% > subscribe(pubsub_server, self(), <<"foo">>, []).
%% ok
%% > subscribe(pubsub_server, self(), <<"foo">>,
%%     [{fastlane, {FastPid, my_serializer, [<<"event1">>]}]).
%% ok
%% @end
-spec subscribe(atom(), pos_integer(), pid(), binary(), [term()]) -> ok.
subscribe(Server, PoolSize, Pid, Topic, Opts) when is_atom(Server) ->
  {ok, {Topics, Pids}} = gen_server:call(
    local_for_pid(Server, Pid, PoolSize),
    {subscribe, Pid, Topic, Opts}
  ),

  Fastlane = ebus_utils:keyfind(fastlane, Opts, nil),
  true = ets:insert(Topics, {Topic, {Pid, Fastlane}}),
  true = ets:insert(Pids, {Pid, Topic}),
  ok.

%% @doc
%% Unsubscribes the pid from the topic.
%%
%% * `Server`: The registered server name or pid
%% * `Pid`: The subscriber pid
%% * `Topic`: The string topic, for example <<"users:123">>
%%
%% Examples:
%%
%% > unsubscribe(pubsub_server, self(), <<"foo">>).
%% ok
%% @end
-spec unsubscribe(atom(), pos_integer(), pid(), binary()) -> ok.
unsubscribe(Server, PoolSize, Pid, Topic) when is_atom(Server) ->
  {LocalServer, GCServer} = pools_for_shard(
    pid_to_shard(Pid, PoolSize), Server
  ),
  ok = ebus_ps_gc:unsubscribe(Pid, Topic, LocalServer, GCServer).

%% @doc
%% Sends a message to all subscribers of a topic.
%%
%% * `Server`: The registered server name or pid
%% * `Topic`: The string topic, for example <<"users:123">>
%%
%% Examples:
%%
%% > broadcast(pubsub_server, self(), <<"foo">>).
%% ok
%% > broadcast(pubsub_server, none, <<"bar">>).
%% ok
%% @end
-spec broadcast(atom(), pos_integer(), pid(), binary(), term()) -> ok.
broadcast(Server, 1, From, Topic, Msg) when is_atom(Server) ->
  do_broadcast(Server, 0, From, Topic, Msg),
  ok;
broadcast(Server, PoolSize, From, Topic, Msg) when is_atom(Server) ->
  Parent = self(),
  ebus_utils:pmap(
    fun(Shard) ->
      do_broadcast(Server, Shard, From, Topic, Msg),
      unlink(Parent)
    end, lists:seq(0, PoolSize - 1)
  ),
  ok.

%% @private
do_broadcast(Server, Shard, From, Topic, #broadcast{event = Event} = Msg) ->
  Reduce = fun
    ({Pid, _Fastlanes}, Cache) when Pid == From ->
      Cache;
    ({Pid, nil}, Cache) ->
      Pid ! Msg,
      Cache;
    ({Pid, {FastlanePid, Serializer, EventIntercepts}}, Cache) ->
      case lists:member(Event, EventIntercepts) of
        true ->
          Pid ! Msg,
          Cache;
        _ ->
          case maps:get(Serializer, Cache, nil) of
            nil ->
              EncodedMsg = Serializer:fastlane(Msg),
              FastlanePid ! EncodedMsg,
              maps:put(Serializer, EncodedMsg, Cache);
            EncodedMsg ->
              FastlanePid ! EncodedMsg,
              Cache
          end
      end
  end,
  Subscribers = subscribers_with_fastlanes(Server, Topic, Shard),
  lists:foldl(Reduce, #{}, Subscribers);
do_broadcast(Server, Shard, From, Topic, Msg) ->
  lists:foreach(
    fun
      (Pid) when Pid == From ->
        noop;
      (Pid) ->
        Pid ! Msg
    end, subscribers_by_shard(Server, Topic, Shard)
  ).

%% @doc
%% Returns a set of subscribers pids for the given topic.
%%
%% * `Server`: The registered server name or pid
%% * `Topic`: The string topic, for example <<"users:123">>
%%
%% Examples:
%%
%% > subscribers(pubsub_server, <<"foo">>).
%% [<0.48.0>, <0.49.0>]
%% @end
-spec subscribers(atom(), pos_integer(), binary()) -> [pid()].
subscribers(Server, PoolSize, Topic) when is_atom(Server) ->
  lists:foldl(
    fun(Shard, Acc) ->
      Acc ++ subscribers_by_shard(Server, Topic, Shard)
    end, [], lists:seq(0, PoolSize - 1)
  ).

%% @doc
%% Returns a set of subscribers pids for the given topic and shard.
%% @see `subscribers/3` for more information.
%% @end
-spec subscribers_by_shard(atom(), binary(), non_neg_integer()) -> [pid()].
subscribers_by_shard(Server, Topic, Shard) when is_atom(Server) ->
  Pids = subscribers_with_fastlanes(Server, Topic, Shard),
  [Pid || {Pid, _Fastlanes} <- Pids].

%% @doc
%% Returns a set of subscribers pids for the given topic and shard  with
%% fastlane tuples.
%% @see `subscribers_by_shard/3` for more information.
%% @end
-spec subscribers_with_fastlanes(
  atom(), binary(), non_neg_integer()
) -> [{pid(), nil | term()}].
subscribers_with_fastlanes(Server, Topic, Shard) when is_atom(Server) ->
  try
    ets:lookup_element(local_for_shard(Shard, Server), Topic, 2)
  catch
    error:badarg -> []
  end.

%% @doc
%% Returns the topic list.
%% This is an expensive and private operation. DO NOT USE IT IN PROD.
%% @end
-spec list(atom(), pos_integer()) -> [binary()].
list(Server, PoolSize) ->
  lists:foldl(
    fun(Shard, Acc) ->
      Acc ++ list_by_shard(Server, Shard)
    end, [], lists:seq(0, PoolSize - 1)
  ).

%% @doc
%% This is an expensive and private operation. DO NOT USE IT IN PROD.
%% @end
-spec list_by_shard(atom(), non_neg_integer()) -> [binary()].
list_by_shard(Server, Shard) when is_atom(Server) ->
  lists:usort(
    ets:select(
      local_for_shard(Shard, Server),
      [{{'$1', '_'}, [], ['$1']}]
    )
  ).

%% @doc
%% This is an expensive and private operation. DO NOT USE IT IN PROD.
%% @end
-spec subscription(atom(), non_neg_integer(), pid()) -> binary().
subscription(Server, PoolSize, Pid) when is_atom(Server) ->
  {_Local, GCServer} = pools_for_shard(
    pid_to_shard(Pid, PoolSize), Server
  ),
  gen_server:call(GCServer, {subscription, Pid}).

-spec local_name(atom(), non_neg_integer()) -> atom().
local_name(Server, Shard) ->
  ebus_utils:build_name([Server, <<"local">>, Shard], <<"_">>).

-spec gc_name(atom(), non_neg_integer()) -> atom().
gc_name(Server, Shard) ->
  ebus_utils:build_name([Server, <<"gc">>, Shard], <<"_">>).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([Local, GC]) ->
  TabOpts = [
    duplicate_bag,
    named_table,
    public,
    {read_concurrency, true},
    {write_concurrency, true}
  ],
  Local = ets:new(Local, TabOpts),
  GC = ets:new(GC, TabOpts),
  process_flag(trap_exit, true),
  {ok, #{topics => Local, pids => GC, gc_server => GC}}.

%% @hidden
handle_call({subscribe, Pid, _Topic, Opts}, _From,
            #{topics := Topics, pids := Pids} = State) ->
  case ebus_utils:keyfind(link, Opts, nil) of
    nil -> ok;
    _   -> link(Pid)
  end,
  erlang:monitor(process, Pid),
  {reply, {ok, {Topics, Pids}}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
handle_info({'DOWN', _Ref, _Type, Pid, _Info},
            #{gc_server := GCServer} = State) ->
  ebus_ps_gc:down(GCServer, Pid),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
local_for_pid(Server, Pid, PoolSize) ->
  local_for_shard(pid_to_shard(Pid, PoolSize), Server).

%% @private
local_for_shard(Shard, Server) ->
  {LocalServer, _GCServer} = pools_for_shard(Shard, Server),
  LocalServer.

%% @private
pools_for_shard(Shard, Server) ->
  [{Shard, {_, _} = Servers}] = ets:lookup(Server, Shard),
  Servers.

%% @private
pid_to_shard(Pid, ShardSize) ->
  pid_id(Pid) rem ShardSize.

%% @private
pid_id(Pid) ->
  Binary = term_to_binary(Pid),
  Prefix = (byte_size(Binary) - 9) * 8,
  <<_:Prefix, Id:32, _:40>> = Binary,
  Id.
