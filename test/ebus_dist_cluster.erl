-module(ebus_dist_cluster).

-export([
  start/1,
  stop/1,
  start_slaves/1,
  stop_slaves/1
]).

-define(PRIMARY, 'primary@127.0.0.1').

%%%===================================================================
%%% API
%%%===================================================================

start(Nodes) ->
  ok = start_primary_node(),
  ok = allow_boot(),
  Slaves = start_slaves(Nodes),
  {ok, Slaves}.

stop(Nodes) ->
  _ = stop_slaves(Nodes -- [?PRIMARY]),
  ok.

start_slaves(Slaves) ->
  start_slaves(Slaves, []).

stop_slaves(Slaves) ->
  stop_slaves(Slaves, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
start_primary_node() ->
  _ = net_kernel:start([?PRIMARY]),
  ok.

%% @private
allow_boot() ->
  _ = erl_boot_server:start([]),
  {ok, IPv4} = inet:parse_ipv4_address("127.0.0.1"),
  erl_boot_server:add_slave(IPv4).

%% @private
start_slaves([], Acc) ->
  lists:usort(Acc);
start_slaves([Node | T], Acc) ->
  start_slaves(T, [spawn_node(Node) | Acc]).

%% @private
spawn_node(NodeName) ->
  Cookie = atom_to_list(erlang:get_cookie()),
  InetLoaderArgs = "-loader inet -hosts 127.0.0.1 -setcookie " ++ Cookie,

  {ok, Pid, Node} = start_node(NodeName, InetLoaderArgs),

  ok = rpc:block_call(Node, code, add_paths, [code:get_path()]),
  {ok, _} = rpc:block_call(Node, application, ensure_all_started, [ebus]),
  {Pid, Node}.

%% @private
node_name(Node) ->
  [Name, _] = binary:split(atom_to_binary(Node, utf8), <<"@">>),
  binary_to_atom(Name, utf8).

%% @private
stop_slaves([], Acc) ->
  lists:usort(Acc);
stop_slaves([Node | T], Acc) ->
  ok = stop_node(Node),
  stop_slaves(T, [Node | Acc]).

-if(?OTP_RELEASE >= 25).
%% @private
start_node(NodeName, InetLoaderArgs) ->
  peer:start(#{name => node_name(NodeName), host => "127.0.0.1", args => [InetLoaderArgs]}).

%% @private
stop_node({Pid, _Node}) -> peer:stop(Pid).
-else.
%% @private
start_node(NodeName, InetLoaderArgs) ->
  {ok, Node} = slave:start("127.0.0.1", node_name(NodeName), InetLoaderArgs),
  {ok, self(), Node}.

%% @private
stop_node({_Pid, Node}) -> slave:stop(Node).
-endif.
