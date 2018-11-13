%%%-------------------------------------------------------------------
%%% @doc
%%% This is an Erlang clone of the original `Phoenix.PubSub.Local'
%%% module.
%%% Copyright (c) 2014 Chris McCord
%%% @reference See
%%% <a href="https://github.com/phoenixframework/phoenix">Phoenix</a>
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

%%%===================================================================
%%% Types
%%%===================================================================

%% @type fastlane() =
%% {FastlanePid :: pid(),
%%  Serializer :: module(),
%%  EventIntercepts :: [term()]}.
%%
%% Fastlane definition.
-type fastlane() :: {
  FastlanePid     :: pid(),
  Serializer      :: module(),
  EventIntercepts :: [term()]
}.

%% @type option() = {link, any()} | {fastlane, fastlane()}.
%%
%% `subscribe/5' function options.
-type option()  :: {link, _} | {fastlane, fastlane()}.

%% @type options() = [option()].
-type options() :: [option()].

-export_type([fastlane/0, option/0, options/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server.
%%
%% <ul>
%% <li>`ServerName': The name to register the server under.</li>
%% </ul>
%% @end
-spec start_link(atom(), atom()) -> gen:start_ret().
start_link(ServerName, GCName) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, GCName], []).

%% @equiv subscribe(Server, PoolSize, Pid, Topic, [])
subscribe(Server, PoolSize, Pid, Topic) ->
  subscribe(Server, PoolSize, Pid, Topic, []).

%% @doc
%% Subscribes the pid to the topic.
%%
%% <ul>
%% <li>`Server': The Pid registered name of the server.</li>
%% <li>`Pid': The subscriber pid to receive pubsub messages.</li>
%% <li>`Topic': The topic to subscribe to, ie: `"users:123"'.</li>
%% <li>`Opts': The optional list of options. See below.</li>
%% </ul>
%%
%% <b>Options:</b>
%% <br/>
%% <ul>
%% <li>`link': links the subscriber to the pubsub adapter.</li>
%% <li>`fastlane': Provides a fastlane path for the broadcasts for
%% `broadcast()' events. The fastlane process is notified of a cached
%% message instead of the normal subscriber. Fastlane handlers must
%% implement `fastlane/1' callbacks which accepts a `broadcast()' struct
%% and returns a fastlaned format for the handler.</li>
%% </ul>
%%
%% Examples:
%%
%% ```
%% > subscribe(pubsub_server, self(), <<"foo">>, []).
%% ok
%% > subscribe(pubsub_server, self(), <<"foo">>,
%%     [{fastlane, {FastPid, my_serializer, [<<"event1">>]}]).
%% ok
%% '''
%% @end
-spec subscribe(atom(), pos_integer(), pid(), binary(), options()) -> ok.
subscribe(Server, PoolSize, Pid, Topic, Opts) when is_atom(Server) ->
  {ok, {Topics, Pids}} = gen_server:call(
    local_for_pid(Server, Pid, PoolSize),
    {subscribe, Pid, Topic, Opts}
  ),
  Fastlane = ebus_common:keyfind(fastlane, Opts),
  true = ets:insert(Topics, {Topic, {Pid, Fastlane}}),
  true = ets:insert(Pids, {Pid, Topic}),
  ok.

%% @doc
%% Unsubscribes the pid from the topic.
%%
%% <ul>
%% <li>`Server': The registered server name or pid.</li>
%% <li>`Pid': The subscriber pid.</li>
%% <li>`Topic': The string topic, for example `<<"users:123">>'.</li>
%% </ul>
%%
%% Example:
%%
%% ```
%% > unsubscribe(pubsub_server, self(), <<"foo">>).
%% ok
%% '''
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
%% <ul>
%% <li>`Server': The registered server name or pid.</li>
%% <li>`Topic': The string topic, for example `<<"users:123">>'.</li>
%% </ul>
%%
%% Examples:
%%
%% ```
%% > broadcast(pubsub_server, self(), <<"foo">>).
%% ok
%% > broadcast(pubsub_server, none, <<"bar">>).
%% ok
%% '''
%% @end
-spec broadcast(atom(), pos_integer(), pid(), binary(), term()) -> ok.
broadcast(Server, 1, From, Topic, Msg) when is_atom(Server) ->
  do_broadcast(Server, 0, From, Topic, Msg),
  ok;
broadcast(Server, PoolSize, From, Topic, Msg) when is_atom(Server) ->
  Parent = self(),
  Tasks = [begin
    shards_task:async(fun() ->
      do_broadcast(Server, Shard, From, Topic, Msg),
      unlink(Parent)
    end)
  end || Shard <- lists:seq(0, PoolSize - 1)],
  lists:foreach(fun(Task) -> shards_task:await(Task) end, Tasks).

%% @private
do_broadcast(Server, Shard, From, Topic,
             #{ebus_t := broadcast, event := Event} = Msg) ->
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
  lists:foreach(fun
    (Pid) when Pid == From ->
      noop;
    (Pid) ->
      Pid ! Msg
  end, subscribers_by_shard(Server, Topic, Shard)).

%% @doc
%% Returns a set of subscribers pids for the given topic.
%%
%% <ul>
%% <li>`Server': The registered server name or pid.</li>
%% <li>`Topic': The string topic, for example `<<"users:123">>'.</li>
%% </ul>
%%
%% Examples:
%%
%% ```
%% > subscribers(pubsub_server, <<"foo">>).
%% [<0.48.0>, <0.49.0>]
%% '''
%% @end
-spec subscribers(atom(), pos_integer(), binary()) -> [pid()].
subscribers(Server, PoolSize, Topic) when is_atom(Server) ->
  lists:foldl(fun(Shard, Acc) ->
    Acc ++ subscribers_by_shard(Server, Topic, Shard)
  end, [], lists:seq(0, PoolSize - 1)).

%% @doc
%% Returns a set of subscribers pids for the given topic and shard.
%% @see subscribers/3.
%% @end
-spec subscribers_by_shard(atom(), binary(), non_neg_integer()) -> [pid()].
subscribers_by_shard(Server, Topic, Shard) when is_atom(Server) ->
  Pids = subscribers_with_fastlanes(Server, Topic, Shard),
  [Pid || {Pid, _Fastlanes} <- Pids].

%% @doc
%% Returns a set of subscribers pids for the given topic and shard  with
%% fastlane tuples.
%% @see subscribers_by_shard/3.
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
%% Returns the topic list for all local shards.
%%
%% <p><font color="red">This is an expensive and private operation.
%% <b> DO NOT USE IT IN PROD</b></font></p>
%% @end
-spec list(atom(), pos_integer()) -> [binary()].
list(Server, PoolSize) ->
  lists:foldl(fun(Shard, Acc) ->
    Acc ++ list_by_shard(Server, Shard)
  end, [], lists:seq(0, PoolSize - 1)).

%% @doc
%% Returns the topic list for the given shard.
%%
%% <p><font color="red">This is an expensive and private operation.
%% <b> DO NOT USE IT IN PROD</b></font></p>
%% @end
-spec list_by_shard(atom(), non_neg_integer()) -> [binary()].
list_by_shard(Server, Shard) when is_atom(Server) ->
  lists:usort(ets:select(
    local_for_shard(Shard, Server),
    [{{'$1', '_'}, [], ['$1']}]
  )).

%% @doc
%% Returns a list of topics which `Pid' is subscribed.
%%
%% <p><font color="red">This is an expensive and private operation.
%% <b> DO NOT USE IT IN PROD</b></font></p>
%% @end
-spec subscription(atom(), non_neg_integer(), pid()) -> [binary()].
subscription(Server, PoolSize, Pid) when is_atom(Server) ->
  {_Local, GCServer} = pools_for_shard(
    pid_to_shard(Pid, PoolSize), Server
  ),
  gen_server:call(GCServer, {subscription, Pid}).

-spec local_name(atom(), non_neg_integer()) -> atom().
local_name(Server, Shard) ->
  ebus_common:build_name([Server, <<"local">>, Shard], <<"_">>).

-spec gc_name(atom(), non_neg_integer()) -> atom().
gc_name(Server, Shard) ->
  ebus_common:build_name([Server, <<"gc">>, Shard], <<"_">>).

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
  case ebus_common:keyfind(link, Opts) of
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
