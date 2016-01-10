%%%-------------------------------------------------------------------
%%% @doc
%%% Process utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_process).

%% API
-export([wait_for_msg/0, wait_for_msg/1]).
-export([messages/0, messages/1, r_messages/1]).
-export([spawn_timer_fun/1]).
-export([spawn_handler/1, spawn_handler/2, spawn_handler/3]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% @type callback() =
%% fun((Msg :: term()) -> any()) |
%% fun((Ctx :: term(), Msg :: term()) -> any()).
-type callback() :: fun((Msg :: term()) -> any()) |
                    fun((Ctx :: term(), Msg :: term()) -> any()).

%% Exported types
-export_type([callback/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv wait_for_msg(infinity)
wait_for_msg() ->
  wait_for_msg(infinity).

%% @doc
%%
%% @end
-spec wait_for_msg(timeout()) -> term() | {error, timeout}.
wait_for_msg(Timeout) ->
  receive
    Msg -> Msg
  after
    Timeout -> {error, timeout}
  end.

%% @equiv process_messages(self())
messages() ->
  messages(self()).

%% @doc
%% Returns all queued messages for the process `Pid'.
%% @end
-spec messages(pid()) -> [term()].
messages(Pid) ->
  {messages, Messages} = erlang:process_info(Pid, messages),
  Messages.

%% @doc
%% Returns all queued messages for the process `Pid', but doesn't
%% matter if process is local or remote.
%% @end
-spec r_messages(pid()) -> [term()].
r_messages(Pid) ->
  {messages, Messages} = rpc:pinfo(Pid, messages),
  Messages.

%% @doc
%% Spawns a linked process that sleeps for the given `Timeout', once
%% timeout expires then process dies.
%% @end
-spec spawn_timer_fun(timeout()) -> pid().
spawn_timer_fun(Timeout) ->
  spawn_link(fun() -> timer:sleep(Timeout) end).

%% @equiv spawn_handler(Fun, nil)
spawn_handler(Fun) ->
  spawn_handler(Fun, nil).

%% @equiv spawn_handler(Fun, Ctx, [])
spawn_handler(Fun, Ctx) ->
  spawn_handler(Fun, Ctx, []).

%% @doc
%% Spawns a process that stays receiving messages, and when a message
%% is received, it calls the given function `Fun'.
%%
%% <ul>
%% <li>`Fun': function callback called when message arrives.</li>
%% <li>`Ctx': Any initial context which is passed as argument to the
%% function callback in order that can be used.</li>
%% <li>`Opts': Spawn process options. See `erlang:spawn_opt/2'.</li>
%% </ul>
%% @see erlang:spawn_opt/2.
%% @end
-spec spawn_handler(
  callback(), term(), [term()]
) -> pid() | {pid(), reference()}.
spawn_handler(Fun, Ctx, Opts) ->
  spawn_opt(fun() -> handle(Fun, Ctx) end, Opts).

%% @private
handle(Fun, Ctx) ->
  receive
    Message ->
      case erlang:fun_info(Fun, arity) of
        {arity, 2} -> Fun(Ctx, Message);
        {arity, 1} -> Fun(Message)
      end,
      handle(Fun, Ctx)
  end.
