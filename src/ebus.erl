%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Andres Bolaños, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2015, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc EBus API.
%%%-------------------------------------------------------------------
-module(ebus).

-behaviour(gen_server).

%% API
-export([new/1, new/2, new/3]).
-export([set_options/1, set_options/2]).
-export([sub/2, sub/3, unsub/2, unsub/3, pub/2, pub/3]).
-export([get_subscribers/1, get_subscribers/2, get_topics/0, get_topics/1]).
-export([dispatch/3, dispatch/4]).

%% Hidden
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% ebus types
-type cmd()      :: sub | unsub | pub | dispatch | get_subscribers | get_topics.
-type topic()    :: any().
-type payload()  :: any().
-type message()  :: {topic(), payload()}.
-type handler()  :: pid().
-type callback() :: {Module :: module(), Function :: atom(), Args :: [any()]}.
-type reason()   :: no_such_topic | no_handler | internal.
-type ebus_ret() :: ok | {error, atom() | {reason(), topic()}}.

%% Exported types
-export_type([cmd/0,
              topic/0,
              payload/0,
              message/0,
              handler/0,
              callback/0,
              ebus_ret/0]).

%% Internal types
-type name()    :: atom().
-type option()  :: {atom(), any()}.
-type options() :: [option()].

%% State
-record(state, {module          :: module(),
                call_timeout    :: non_neg_integer(),
                n               :: non_neg_integer(),
                sub             :: non_neg_integer(),
                unsub           :: non_neg_integer(),
                pub             :: non_neg_integer(),
                dispatch        :: non_neg_integer(),
                get_subscribers :: non_neg_integer(),
                get_topics      :: non_neg_integer()}).

%% Modules
-define(MODULES, [ebus_pg2, ebus_gproc, ebus_dist]).
-define(DEFAULT, ebus_pg2).

%% Server
-define(SERVER, ?MODULE).

%%%===================================================================
%%% Callback API
%%%===================================================================

%% @doc Subscribes the `Handler` to the `Topic`. Once subscription in done
%%      successfully, the `Handler` is able to listen the all events sent
%%      to the `Topic`.
-callback sub(Topic, Handler) -> Response when
  Topic    :: topic(),
  Handler  :: handler(),
  Response :: ebus_ret().

%% @doc Unsubscribe the `Handler` of the `Topic`. Once the operation is done
%%      successfully, the Handler is not able to listen events sent to the
%%      `Topic` any more.
-callback unsub(Topic, Handler) -> Response when
  Topic    :: topic(),
  Handler  :: handler(),
  Response :: ebus_ret().

%% @doc Sends the `Message` to all subscribers of the `Topic`.
-callback pub(Topic, Message) -> Response when
  Topic    :: topic(),
  Message  :: payload(),
  Response :: ebus_ret().

%% @doc Returns a list with all subscribers to the `Topic`.
-callback get_subscribers(Topic) -> Response when
  Topic    :: topic(),
  Response :: [handler()].

%% @doc Returns a list with all registered topics.
-callback get_topics() -> Response when
  Response :: [topic()].

%% @doc Sends the `Message` to `Handler`, which is subscribed to `Topic`.
-callback dispatch(Topic, Message, Handler) -> Response when
  Topic    :: topic(),
  Message  :: payload(),
  Handler  :: handler(),
  Response :: ebus_ret().

%%%===================================================================
%%% Hidden API
%%%===================================================================

%% @hidden
-spec start_link(name(), module(), options()) -> gen:start_ret().
start_link(Name, Module, Opts) ->
  gen_server:start_link({local, Name}, ?MODULE, [Module, Opts], []).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(name()) -> gen:start_ret().
new(Name) ->
  new(Name, ebus_pg2, []).

-spec new(name(), module()) -> gen:start_ret().
new(Name, Module) ->
  new(Name, Module, []).

-spec new(name(), module(), options()) -> supervisor:startchild_ret().
new(Name, Module, Opts) ->
  true = Name /= local andalso Name /= global,
  ebus_sup:start_child([Name, Module, Opts]).

-spec set_options(options()) -> ok.
set_options(Opts) ->
  gen_server:call(?SERVER, {set_options, Opts}).

-spec set_options(name(), options()) -> ok.
set_options(Name, Opts) ->
  gen_server:call(Name, {set_options, Opts}).

-spec sub(topic(), handler()) -> ebus_ret().
sub(Topic, Handler) ->
  gen_server:call(?SERVER, {sub, Topic, Handler}).

-spec sub(name(), topic(), handler()) -> ebus_ret().
sub(Name, Topic, Handler) ->
  gen_server:call(Name, {sub, Topic, Handler}).

-spec unsub(topic(), handler()) -> ebus_ret().
unsub(Topic, Handler) ->
  gen_server:call(?SERVER, {unsub, Topic, Handler}).

-spec unsub(name(), topic(), handler()) -> ebus_ret().
unsub(Name, Topic, Handler) ->
  gen_server:call(Name, {unsub, Topic, Handler}).

-spec pub(topic(), payload()) -> ebus_ret().
pub(Topic, Message) ->
  gen_server:call(?SERVER, {pub, Topic, Message}).

-spec pub(name(), topic(), payload()) -> ebus_ret().
pub(Name, Topic, Message) ->
  gen_server:call(Name, {pub, Topic, Message}).

-spec get_subscribers(topic()) -> [handler()].
get_subscribers(Topic) ->
  gen_server:call(?SERVER, {get_subscribers, Topic}).

-spec get_subscribers(name(), topic()) -> [handler()].
get_subscribers(Name, Topic) ->
  gen_server:call(Name, {get_subscribers, Topic}).

-spec get_topics() -> [topic()].
get_topics() ->
  gen_server:call(?SERVER, get_topics).

-spec get_topics(name()) -> [topic()].
get_topics(Name) ->
  gen_server:call(Name, get_topics).

-spec dispatch(topic(), payload(), handler()) -> ebus_ret().
dispatch(Topic, Message, Handler) ->
  gen_server:call(?SERVER, {dispatch, Topic, Message, Handler}).

-spec dispatch(name(), topic(), payload(), handler()) -> ebus_ret().
dispatch(Name, Topic, Message, Handler) ->
  gen_server:call(Name, {dispatch, Topic, Message, Handler}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([Module, Opts]) ->
  NewMod = resolve_module(Module),
  State = parse_options(Opts, #state{module = NewMod}),
  {ok, State}.

%% @hidden
handle_call({set_options, Opts}, _From, State) ->
  NState = parse_options(Opts, State),
  {reply, ok, NState};
handle_call({sub, T, H},
            _From, #state{module = ebus_dist, n = N, sub = Q} = S0) ->
  Reply = ebus_dist:sub(T, H, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({sub, T, H}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:sub(T, H),
  {reply, Reply, S0};
handle_call({unsub, T, H},
            _From,
            #state{module = ebus_dist, n = N, unsub = Q} = S0) ->
  Reply = ebus_dist:unsub(T, H, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({unsub, T, H}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:unsub(T, H),
  {reply, Reply, S0};
handle_call({pub, T, M},
            _From,
            #state{module = ebus_dist, n = N, pub = Q} = S0) ->
  Reply = ebus_dist:pub(T, M, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({pub, T, M}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:pub(T, M),
  {reply, Reply, S0};
handle_call({get_subscribers, T},
            _From,
            #state{module = ebus_dist, n = N, get_subscribers = Q} = S0) ->
  Reply = ebus_dist:get_subscribers(T, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({get_subscribers, T}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:get_subscribers(T),
  {reply, Reply, S0};
handle_call(get_topics,
            _From,
            #state{module = ebus_dist, n = N, get_topics = Q} = S0) ->
  Reply = ebus_dist:get_topics(dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call(get_topics, _From, #state{module = Mod} = S0) ->
  Reply = Mod:get_topics(),
  {reply, Reply, S0};
handle_call({dispatch, T, M, H},
            _From,
            #state{module = ebus_dist, n = N, dispatch = Q} = S0) ->
  Reply = ebus_dist:dispatch(T, M, H, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({dispatch, T, M, H}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:dispatch(T, M, H),
  {reply, Reply, S0}.

%% @hidden
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
parse_options([], State) ->
  State;
parse_options([{call_timeout, T} | Opts], State) when is_integer(T) ->
  parse_options(Opts, State#state{call_timeout = T});
parse_options([{n, N} | Opts], State) when is_integer(N) ->
  parse_options(Opts, State#state{n = N});
parse_options([{sub, Q} | Opts], State) when is_integer(Q) ->
  parse_options(Opts, State#state{sub = Q});
parse_options([{unsub, Q} | Opts], State) when is_integer(Q) ->
  parse_options(Opts, State#state{unsub = Q});
parse_options([{pub, Q} | Opts], State) when is_integer(Q) ->
  parse_options(Opts, State#state{pub = Q});
parse_options([{dispatch, Q} | Opts], State) when is_integer(Q) ->
  parse_options(Opts, State#state{dispatch = Q});
parse_options([{get_subscribers, Q} | Opts], State) when is_integer(Q) ->
  parse_options(Opts, State#state{get_subscribers = Q});
parse_options([{get_topics, Q} | Opts], State) when is_integer(Q) ->
  parse_options(Opts, State#state{get_topics = Q});
parse_options([_Opt | Opts], State) ->
  parse_options(Opts, State).

%% @private
dist_opts(undefined, undefined) ->
  [];
dist_opts(N, undefined) ->
  [{n, N}, {q, round(N/2)}];
dist_opts(undefined, Q) ->
  [{n, ((Q * 2) - 1)}, {q, Q}];
dist_opts(N, Q) ->
  [{n, N}, {q, Q}].

%% @private
resolve_module(Module) ->
  case lists:member(Module, ?MODULES) of
    true  -> Module;
    false -> ?DEFAULT
  end.
