%%%-------------------------------------------------------------------
%%% @doc
%%% This is an Erlang clone of the original `Phoenix.PubSub' module.
%%% Copyright (c) 2014 Chris McCord
%%% @reference See
%%% <a href="https://github.com/phoenixframework/phoenix">Phoenix</a>
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_ps).

%% API
-export([
  subscribe/3,
  subscribe/4,
  unsubscribe/3,
  broadcast/3,
  broadcast_from/4,
  subscribers/2,
  list/1
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type options() :: ebus_ps_local:options().

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv subscribe(Server, Pid, Topic, [])
subscribe(Server, Pid, Topic) ->
  subscribe(Server, Pid, Topic, []).

%% @doc
%% Subscribes the pid to the PubSub adapter's topic.
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
%% and returns a fastlaned format for the handler. For example:</li>
%% <br/>
%%   ```
%%     ebus_ps:subscribe(
%%       my_pubsub_server, self(), <<"topic1">>,
%%       [{fastlane, {FastPid, my_serializer, [<<"event1">>]}).`
%%   '''
%% </ul>
%% @end
-spec subscribe(atom(), pid(), binary(), options()) -> ok | {error, term()}.
subscribe(Server, Pid, Topic, Opts) when is_atom(Server) ->
  call(Server, subscribe, [Pid, Topic, Opts]).

%% @doc
%% Unsubscribes the pid from the PubSub adapter's topic.
%% @end
-spec unsubscribe(atom(), pid(), binary()) -> ok | {error, term()}.
unsubscribe(Server, Pid, Topic) when is_atom(Server) ->
  call(Server, unsubscribe, [Pid, Topic]).

%% @doc
%% Broadcasts message on given topic.
%% @end
-spec broadcast(atom(), binary(), term()) -> ok | {error, term()}.
broadcast(Server, Topic, Msg) when is_atom(Server) ->
  call(Server, broadcast, [none, Topic, Msg]).

%% @doc
%% Broadcasts message to all but `FromPid' on given topic.
%% @end
-spec broadcast_from(atom(), pid(), binary(), term()) -> ok | {error, term()}.
broadcast_from(Server, FromPid, Topic, Msg)
    when is_atom(Server), is_pid(FromPid) ->
  call(Server, broadcast, [FromPid, Topic, Msg]).

%% @doc
%% Returns a set of subscribers pids for the given topic.
%%
%% <ul>
%% <li>`Server': The registered server name or pid.</li>
%% <li>`Topic': The string topic, for example `<<"users:123">>'.</li>
%% </ul>
%%
%% Example:
%%
%% ```
%% > subscribers(pubsub_server, <<"foo">>).
%% [<0.48.0>, <0.49.0>]
%% '''
%% @end
-spec subscribers(atom(), binary()) -> [pid()].
subscribers(Server, Topic) when is_atom(Server) ->
  call(Server, subscribers, [Topic]).

%% @doc
%% Returns the topic list.
%% This is an expensive and private operation. DO NOT USE IT IN PROD.
%% @end
-spec list(atom()) -> [binary()].
list(Server) when is_atom(Server) ->
  call(Server, list, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
call(Server, Kind, Args) ->
  [{Kind, Module, Head}] = ets:lookup(Server, Kind),
  apply(Module, Kind, Head ++ Args).
