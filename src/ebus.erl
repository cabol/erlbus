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
-export([get_subscribers/1, get_subscribers/2]).
-export([get_channels/0, get_channels/1]).
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
-type cmd()      :: sub | unsub | pub | dispatch |
                    get_subscribers | get_channels.
-type channel()  :: any().
-type payload()  :: any().
-type message()  :: {channel(), payload()}.
-type handler()  :: pid().
-type callback() :: {Module :: module(), Function :: atom(), Args :: [any()]}.
-type reason()   :: no_such_channel | no_handler | internal.
-type ebus_ret() :: ok | {error, atom() | {reason(), channel()}}.

%% Exported types
-export_type([cmd/0,
              channel/0,
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
                get_channels    :: non_neg_integer()}).

%% Modules
-define(MODULES, [ebus_pg2, ebus_gproc, ebus_dist]).
-define(DEFAULT, ebus_pg2).

%% Server
-define(SERVER, ?MODULE).

%%%===================================================================
%%% Callback API
%%%===================================================================

%% @doc Subscribes the `Handler` to the `Channel`. Once subscription in done
%%      successfully, the `Handler` is able to listen the all events sent
%%      to the `Channel`.
-callback sub(Channel, Handler) -> Response when
  Channel  :: channel(),
  Handler  :: handler() | [handler()],
  Response :: ebus_ret().

%% @doc Unsubscribe the `Handler` of the `Channel`. Once the operation is done
%%      successfully, the Handler is not able to listen events sent to the
%%      `Channel` any more.
-callback unsub(Channel, Handler) -> Response when
  Channel  :: channel(),
  Handler  :: handler() | [handler()],
  Response :: ebus_ret().

%% @doc Sends the `Message` to all subscribers of the `Channel`.
-callback pub(Channel, Message) -> Response when
  Channel  :: channel(),
  Message  :: payload(),
  Response :: ebus_ret().

%% @doc Returns a list with all subscribers to the `Channel`.
-callback get_subscribers(Channel) -> Response when
  Channel  :: channel(),
  Response :: [handler()].

%% @doc Returns a list with all registered channels.
-callback get_channels() -> Response when
  Response :: [channel()].

%% @doc Sends the `Message` to `Handler`, which is subscribed to `Channel`.
-callback dispatch(Channel, Message, Handler) -> Response when
  Channel  :: channel(),
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

-spec sub(channel(), handler() | [handler()]) -> ebus_ret().
sub(Channel, Handler) ->
  gen_server:call(?SERVER, {sub, Channel, Handler}).

-spec sub(name(), channel(), handler() | [handler()]) -> ebus_ret().
sub(Name, Channel, Handler) ->
  gen_server:call(Name, {sub, Channel, Handler}).

-spec unsub(channel(), handler() | [handler()]) -> ebus_ret().
unsub(Channel, Handler) ->
  gen_server:call(?SERVER, {unsub, Channel, Handler}).

-spec unsub(name(), channel(), handler() | [handler()]) -> ebus_ret().
unsub(Name, Channel, Handler) ->
  gen_server:call(Name, {unsub, Channel, Handler}).

-spec pub(channel(), payload()) -> ebus_ret().
pub(Channel, Message) ->
  gen_server:call(?SERVER, {pub, Channel, Message}).

-spec pub(name(), channel(), payload()) -> ebus_ret().
pub(Name, Channel, Message) ->
  gen_server:call(Name, {pub, Channel, Message}).

-spec get_subscribers(channel()) -> [handler()].
get_subscribers(Channel) ->
  gen_server:call(?SERVER, {get_subscribers, Channel}).

-spec get_subscribers(name(), channel()) -> [handler()].
get_subscribers(Name, Channel) ->
  gen_server:call(Name, {get_subscribers, Channel}).

-spec get_channels() -> [channel()].
get_channels() ->
  gen_server:call(?SERVER, get_channels).

-spec get_channels(name()) -> [channel()].
get_channels(Name) ->
  gen_server:call(Name, get_channels).

-spec dispatch(channel(), payload(), handler()) -> ebus_ret().
dispatch(Channel, Message, Handler) ->
  gen_server:call(?SERVER, {dispatch, Channel, Message, Handler}).

-spec dispatch(name(), channel(), payload(), handler()) -> ebus_ret().
dispatch(Name, Channel, Message, Handler) ->
  gen_server:call(Name, {dispatch, Channel, Message, Handler}).

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
handle_call({sub, Ch, H},
            _From, #state{module = ebus_dist, n = N, sub = Q} = S0) ->
  Reply = ebus_dist:sub(Ch, H, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({sub, Ch, H}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:sub(Ch, H),
  {reply, Reply, S0};
handle_call({unsub, Ch, H},
            _From,
            #state{module = ebus_dist, n = N, unsub = Q} = S0) ->
  Reply = ebus_dist:unsub(Ch, H, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({unsub, Ch, H}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:unsub(Ch, H),
  {reply, Reply, S0};
handle_call({pub, Ch, M},
            _From,
            #state{module = ebus_dist, n = N, pub = Q} = S0) ->
  Reply = ebus_dist:pub(Ch, M, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({pub, Ch, M}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:pub(Ch, M),
  {reply, Reply, S0};
handle_call({get_subscribers, Ch},
            _From,
            #state{module = ebus_dist, n = N, get_subscribers = Q} = S0) ->
  Reply = ebus_dist:get_subscribers(Ch, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({get_subscribers, Ch}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:get_subscribers(Ch),
  {reply, Reply, S0};
handle_call(get_channels,
            _From,
            #state{module = ebus_dist, n = N, get_channels = Q} = S0) ->
  Reply = ebus_dist:get_channels(dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call(get_channels, _From, #state{module = Mod} = S0) ->
  Reply = Mod:get_channels(),
  {reply, Reply, S0};
handle_call({dispatch, Ch, M, H},
            _From,
            #state{module = ebus_dist, n = N, dispatch = Q} = S0) ->
  Reply = ebus_dist:dispatch(Ch, M, H, dist_opts(N, Q)),
  {reply, Reply, S0};
handle_call({dispatch, Ch, M, H}, _From, #state{module = Mod} = S0) ->
  Reply = Mod:dispatch(Ch, M, H),
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
parse_options([{get_channels, Q} | Opts], State) when is_integer(Q) ->
  parse_options(Opts, State#state{get_channels = Q});
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
