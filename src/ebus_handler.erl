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
%%% @doc EBus Handler.
%%%-------------------------------------------------------------------
-module(ebus_handler).

-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([new/1, new/2, new/3, delete/1]).
-export([new_pool/3, new_pool/4, new_pool/5]).
-export([get_module/1, get_context/1]).
-export([new_anonymous/1]).
-export([status/1]).

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

%% State
-record(state, {module :: atom(), context :: any(), pool :: atom()}).

%% Types
-type context()    :: any().
-type pool_opt()   :: {name, atom()} | {size, integer()}.
-type option()     :: {monitors, [pid()]} | {pool, [pool_opt()]}.
-type options()    :: [option()].
-type handle_fun() :: fun((ebus:topic(), ebus:payload()) -> ok).
-type status()     :: exiting | garbage_collecting | waiting | running |
                      runnable | suspended.

%%%===================================================================
%%% Callback API
%%%===================================================================

%% @doc This function handles incoming messages/events.
-callback handle_msg(Message, Context) -> Response when
  Message  :: ebus:message(),
  Context  :: context(),
  Response :: any().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(module(), context(), options()) -> gen:start_ret().
start_link(Module, Context, Opts) ->
  gen_server:start_link(?MODULE, [Module, Context, Opts], []).

-spec new(module()) -> ebus:handler().
new(Module) ->
  new(Module, undefined).

-spec new(module(), context()) -> ebus:handler().
new(Module, Context) ->
  new(Module, Context, []).

-spec new(module(), context(), options()) -> ebus:handler().
new(Module, Context, Opts) ->
  case start_link(Module, Context, Opts) of
    {error, Reason} ->
      throw(Reason);
    {ok, Pid} ->
      Pid
  end.

-spec delete(ebus:handler()) -> ok.
delete(Handler) ->
  gen_server:cast(Handler, exit).

-spec new_pool(atom(), integer(), module()) -> ebus:handler().
new_pool(Name, Size, Module) ->
  new_pool(Name, Size, Module, undefined).

-spec new_pool(atom(), integer(), module(), context()) -> ebus:handler().
new_pool(Name, Size, Module, Context) ->
  new_pool(Name, Size, Module, Context, []).

-spec new_pool(
  atom(), integer(), module(), context(), options()
) -> ebus:handler().
new_pool(Name, Size, Module, Context, Opts) ->
  PoolArgs = [{name, {local, Name}},
              {worker_module, ebus_worker},
              {size, Size}],
  {ok, Pool} = poolboy:start_link(PoolArgs, [Module, Context]),
  new(Module, Context, [{pool, Pool} | Opts]).

-spec get_module(ebus:handler()) -> any().
get_module(Handler) ->
  gen_server:call(Handler, get_mod).

-spec get_context(ebus:handler()) -> any().
get_context(Handler) ->
  gen_server:call(Handler, get_ctx).

-spec new_anonymous(handle_fun()) -> ebus:handler().
new_anonymous(Fun) ->
  spawn_link(fun() -> anonymous_handler(Fun) end).

-spec status(ebus:handler()) -> undefined | {status, status()}.
status(Handler) ->
  process_info(Handler, status).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([Module, Context, Opts]) ->
  State = parse_options(Opts, #state{module = Module, context = Context}),
  {ok, State}.

%% @private
handle_call(get_mod, _From, #state{module = Module} = State) ->
  {reply, Module, State};
handle_call(get_ctx, _From, #state{context = Ctx} = State) ->
  {reply, Ctx, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
handle_cast(exit, #state{pool = Pool} = State) when is_pid(Pool) ->
  poolboy:stop(Pool),
  {stop, normal, State};
handle_cast(exit, State) ->
  {stop, normal, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% Handles all incoming messages.
handle_info({ebus, Event}, #state{pool = Pool} = State) when is_pid(Pool) ->
  Worker = poolboy:checkout(Pool),
  ok = gen_server:cast(Worker, {handle_msg, Event, Pool}),
  {noreply, State};
handle_info({ebus, Event}, #state{module = Mod, context = Ctx} = State) ->
  Mod:handle_msg(Event, Ctx),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% Parse options
parse_options([], State) ->
  State;
parse_options([{monitors, Val} | Opts], State) ->
  lists:foreach(fun(I) -> erlang:monitor(process, I) end, Val),
  parse_options(Opts, State);
parse_options([{pool, Pool} | Opts], State) ->
  parse_options(Opts, State#state{pool = Pool});
parse_options([{_, _} | Opts], State) ->
  parse_options(Opts, State).

%% @private
anonymous_handler(Fun) ->
  receive
    {ebus, {Topic, Msg}} ->
      Fun(Topic, Msg),
      anonymous_handler(Fun);
    exit ->
      ok
  end.
