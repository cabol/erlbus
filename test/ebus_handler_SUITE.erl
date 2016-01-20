-module(ebus_handler_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

%% Tests
-export([t_handler/1, t_callback_handler/1]).

%% Others
-export([my_callback/2]).

-define(TAB, ebus_test).

%%%===================================================================
%%% Common Test
%%%===================================================================

all() -> [t_handler, t_callback_handler].

init_per_suite(Config) ->
  ebus:start(),
  Config.

end_per_suite(Config) ->
  ebus:stop(),
  Config.

init_per_testcase(_, Config) ->
  TabId = ets:new(?TAB, [duplicate_bag, public, named_table]),
  [{table,TabId} | Config].

end_per_testcase(_, Config) ->
  ets:delete(?TAB),
  Config.

my_callback(Msg, Args) ->
  ets:insert(?TAB, {ebus_common:build_name([Msg, Args]), Msg}).

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

t_handler(_Config) ->
  % check topics
  [] = ebus:topics(),

  % callback 1
  CB1 = fun(Msg) ->
    ets:insert(?TAB, {ebus_common:build_name([Msg]), Msg})
  end,

  % callback 2
  CB2 = fun(Msg, Arg1, Arg2) ->
    ets:insert(?TAB, {ebus_common:build_name([Msg, Arg1, Arg2]), Msg})
  end,

  % create some handlers
  {H1, Ref1} = ebus_proc:spawn_handler(CB1, [], [monitor]),
  H2 = ebus_proc:spawn_handler(CB2, [<<"H2">>, h2]),

  % subscribe local process
  ok = ebus:sub(H1, <<"foo">>),
  ok = ebus:sub(H2, <<"foo">>),
  ok = ebus:sub(H2, <<"bar">>),

  % publish message
  ebus:pub(<<"foo">>, <<"M1">>),
  timer:sleep(500),

  % check received messages
  [{_, M11}] = ets:lookup(?TAB, key([<<"M1">>])),
  <<"M1">> = M11,
  [{_, M12}] = ets:lookup(?TAB, key([<<"M1">>, <<"H2">>, h2])),
  <<"M1">> = M12,

  % publish message
  ebus:pub(<<"bar">>, <<"M2">>),
  timer:sleep(500),

  % check received messages
  [] = ets:lookup(?TAB, key([<<"M2">>])),
  [{_, M22}] = ets:lookup(?TAB, key([<<"M2">>, <<"H2">>, h2])),
  <<"M2">> = M22,

  ebus:unsub(H2, <<"bar">>),

  % publish message
  ebus:pub(<<"bar">>, <<"M3">>),
  timer:sleep(500),

  % check received messages
  [] = ets:lookup(?TAB, key([<<"M3">>])),
  [] = ets:lookup(?TAB, key([<<"M3">>, <<"H2">>, h2])),

  % check subscribers
  2 = length(ebus:subscribers(<<"foo">>)),
  0 = length(ebus:subscribers(<<"bar">>)),

  % kill handlers and check
  exit(H1, kill),
  {'DOWN', Ref1, _, _, _} = ebus_proc:wait_for_msg(5000),
  timer:sleep(500),
  1 = length(ebus:subscribers(<<"foo">>)),

  ct:print("\e[1;1m t_handler: \e[0m\e[32m[OK] \e[0m"),
  ok.

t_callback_handler(_Config) ->
  % callbacks
  CB1 = fun(Msg) ->
    ets:insert(?TAB, {ebus_common:build_name([Msg]), Msg})
  end,
  CB2 = fun(Msg, Args) ->
    ets:insert(?TAB, {ebus_common:build_name([Msg, Args]), Msg})
  end,

  % create some handlers
  H1 = ebus_proc:spawn_callback_handler(?MODULE, my_callback, [<<"H1">>]),
  H2 = ebus_proc:spawn_handler(CB2, [<<"H2">>]),
  H3 = ebus_proc:spawn_handler(CB1),

  % subscribe local process
  [ok, ok, ok] = [ebus:sub(H, <<"foo">>) || H <- [H1, H2, H3]],

  % publish message
  ebus:pub(<<"foo">>, <<"M1">>),
  timer:sleep(500),

  % check received messages
  [{_, M11}] = ets:lookup(?TAB, key([<<"M1">>, <<"H1">>])),
  <<"M1">> = M11,
  [{_, M12}] = ets:lookup(?TAB, key([<<"M1">>, <<"H2">>])),
  <<"M1">> = M12,
  [{_, M13}] = ets:lookup(?TAB, key([<<"M1">>])),
  <<"M1">> = M13,

  ct:print("\e[1;1m t_callback_handler: \e[0m\e[32m[OK] \e[0m"),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

key(L) -> ebus_common:build_name(L).
