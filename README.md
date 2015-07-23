<img src="http://wallpaperssfree.com/wp-content/uploads/2013/05/world-fastest-train-hd-wallpapers.png" height="200" width="100%" />

ErlBus
======

Message / Event Bus written in Erlang.

__Authors:__ Carlos Andres Bolaños R.A. ([`candres@niagara.io`](mailto:candres@niagara.io)).

A new way to build soft real-time and high scalable messaging-based applications, not centralized but distributed!

See also: [WEST](https://github.com/cabol/west).


Building ErlBus
---------------

Assuming you have a working Erlang installation (recommended 17 or later), building **ErlBus** should be as simple as:

    $ git clone https://github.com/cabol/erlbus.git
    $ cd erlbus
    $ make


Note
----

**ErlBus** has 3 dependencies: `gproc`, `poolboy` and `riak_core`. But the last one `riak_core` is fetched
on-demand, when you want to run **ErlBus** in distributed mode, inheriting all `riak_core` benefits.
Learn more about [Riak Core](https://github.com/basho/riak_core).

In this scenario, **ErlBus** runs
with `gproc` locally on each node, and `riak_core` on top of it, managing the cluster and task/command
distribution.

- To enable fetching of `riak_core`, export the OS environment variable `EBUS_DIST=true` (this can be done
  e.g. from a GNU Makefile).


Introduction
------------

**ErlBus** is a very lightweight and simple approach to build messaging-based apps. Messaging infrastructure
that provides: Publish/Subscribe, Point-To-Point, Event Driven Consumer (Message Handler), Task Executor, etc.


Quick Start Example
-------------------

Start an Erlang console:

    $ erl -pa ebin deps/*/ebin

Once into the erlang console:

```erlang
% Start ebus
application:start(ebus).
ok

% Create anonymous function to be invoked by handlers
F = fun(Channel, Msg) -> io:format("Channel: ~p - Msg: ~p~n", [Channel, Msg]) end.
#Fun<erl_eval.12.90072148>

% Create anonymous handlers
MH1 = ebus_handler:new_anonymous(F).
<0.50.0>
MH2 = ebus_handler:new_anonymous(F).
<0.52.0>

% Subscribe them to channel ch1
% Note that `ebus:sub/2,3` can receive either a single handler or a list of them
ebus:sub(ch1, [MH1, MH2]).
ok

% Let's publish a message to 'ch1'
ebus:pub(ch1, "Hello!").
Channel: ch1 - Msg: "Hello!"
Channel: ch1 - Msg: "Hello!"
ok

% Another handler
F2 = fun(Channel, Msg) -> io:format("OTHER -- Channel: ~p - Msg: ~p~n", [Channel, Msg]) end.
#Fun<erl_eval.12.90072148>
MH3 = ebus_handler:new_anonymous(F2).
<0.54.0>

% Subscribe the other handler 'MH3' to ch2
ebus:sub(ch2, MH3).
ok

% Publish to 'ch2'
ebus:pub(ch2, "Hello other!").
OTHER -- Channel: ch2 - Msg: "Hello other!"
ok

% Unsubscribe 'MH2' from ch1
% Note that `ebus:unsub/2,3` can also receive a single handler (this case) or a list of them
ebus:unsub(ch1, MH2).
ok

% Publish again to 'ch1'
ebus:pub(ch1, "Hello again!").
Channel: ch1 - Msg: "Hello again!"
ok
```

> **Note:**

> - You may have noticed that is not necessary additional steps/calls to create/delete a channel,
    this is automatically handled by `ebus`, so you don't worry about it!

Now, let's make it more fun, start two Erlang consoles, first one:

    $ erl -name node1@127.0.0.1 -setcookie ebus -pa ebin deps/*/ebin

The second one:

    $ erl -name node2@127.0.0.1 -setcookie ebus -pa ebin deps/*/ebin

Then what we need to do is put these Erlang nodes in cluster, so from any of them send a ping
to the other:

```erlang
% From node1 ping node2
net_adm:ping('node2@127.0.0.1').
pong
```

Excellent, we have both nodes in cluster, thanks to the beauty of [Distributed Erlang](http://www.erlang.org/doc/reference_manual/distributed.html).
So, let's repeat the above exercise but now in two nodes. In both nodes start `ebus`:

```erlang
% Start ebus
application:start(ebus).
ok
```

Then in `node1` create a handler and subscription to a channel:

```erlang
% Anonymous handler function
F = fun(Channel, Msg) -> io:format("Channel: ~p - Msg: ~p~n", [Channel, Msg]) end.
#Fun<erl_eval.12.90072148>

% Subscribe a handler
ebus:sub(ch1, ebus_handler:new_anonymous(F)).
ok
```

Repeat the same thing above in `node2`.

Once you have handlers subscribed to the same channel in both nodes, publish some messages from
any node:

```erlang
% Publish message
ebus:pub(ch1, "Hi!").
Channel: ch1 - Msg: "Hi!"
ok
```

And in the other node you will see that message has arrived too:

```erlang
Channel: ch1 - Msg: "Hi!"
ok
```

So far, so good! Let's continue!


Basic Pub/Sub Example
---------------------

Previously we saw a simple example how Pub/Sub works, but using anonymous handlers, which aren't bad
in case of any demo or simple test. But the right way would be create your own message handler, using
the `ebus_handler` beahvior. Because in this way, your handler will be part of the supervision tree,
and you will be able to use other features too, that we'll cover later.

First, we have to create an Erlang module to implement the behavior `ebus_handler`, which defines a
callback to handling message logic: `handle_msg({Channel, Payload}, Context)`, where:

- `Channel` is the logical mechanism that allows communicate two or more endpoints each other
  (either Pub/Sub or Point-to-Point) through messages.
- `Payload` is the message itself, the content af what you published or dispatched.
- `Context` is an optional parameter that you can pass in the moment of the handler creation,
   and you want to be able to recovered at the moment of the `handle_msg` invocation.


### my_handler.erl

```erlang
-module(my_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({Channel, Msg}, Context) ->
  io:format("[Pid: ~p][Channel: ~p][Msg: ~p][Ctx: ~p]~n",
            [self(), Channel, Msg, Context]).
```

Once you have compiled your module(s) and started an Erlang console:

```erlang
% Start ebus
application:start(ebus).
ok

% Create a new handler, passing a context as argument
% In this the context is a simple binary string with the name of the handler,
% but it can be anything that you want (tuple, record, map, etc.)
MH1 = ebus_handler:new(my_handler, <<"MH1">>).
<0.49.0>

% From here, everything is the same as previous example
% Subscribe the handler to some channel
ebus:sub(my_channel, MH1).
ok

% Now the handler is ready to receive and process messages
% Publish a message/event
ebus:pub(my_channel, "Hello!").
ok
```

Again, you can start multiple Erlang nodes, put them in cluster, start `ebus` on each one,
and now you have `ebus` running in distributed fashion, it's extremely easy, you have not to do
anything at all.


Point-To-Point Example
----------------------

The great thing here is that you don't need something special to implement a point-to-point behavior.
Is as simple as this:

```erlang
ebus:dispatch(ch1, "Hi!", MyHandler).
```

Instead of call `ebus:pub(Channel, Message)`, you call `ebus:dispatch(Channel, Message, Handler)`,
and the only difference is that you have to provide the `Handler` which will receive the message.
The reason of this is that you're free to implement your scheduling/dispatching strategy. Also,
you can use `ebus_util:get_best_pid(ListOfHandlers)` to find an available handler. For example:

```erlang
%% Start ebus
application:start(ebus).
ok

%% Create some handlers
MH1 = ebus_handler:new(my_handler, <<"MH1">>).
<0.47.0>
MH2 = ebus_handler:new(my_handler, <<"MH2">>).
<0.48.0>
MH3 = ebus_handler:new(my_handler, <<"MH3">>).
<0.49.0>

%% Subscribe created handlers
ebus:sub(my_channel, [MH1, MH2, MH3]).
ok

%% Get the subscribed handlers
Handlers = ebus:subscribers(my_channel).
[<0.47.0>, <0.48.0>, <0.49.0>]

%% Find an available handler
Handler = ebus_util:get_best_pid(Handlers).
<0.47.0>

ebus:dispatch(my_channel, "Hi!", Handler).
Channel: ch1 - Msg: "Hi!"
ok
```

> **Note:**

> - The example above, assumes that you're working with the previous compiled handler `my_hanlder.erl`.


Task Executors (worker pool)
----------------------------

Suppose now that you have a handler that takes a while processing each message/event, so it will
be blocked until complete the task, and for some scenarios would be unthinkable. Therefore,
`ebus_handler` module gives you the option to create a pool of workers attached to your handler,
and is totally transparent to you.

```erlang
% Start ebus
application:start(ebus).
ok

% Create a handler with a worker pool (3 workers)
HandlerPool = ebus_handler:new_pool(my_pool_1, 3, my_handler).
<0.49.0>

% And that's it, now the load will be distributed among the workers
% From here everything is as previously
% Finally, let's subscribe this new handler with workers to some channel
ebus:sub(my_channel, HandlerPool).
ok
```

> **Note:**

> - Another way to get a point-to-point behavior is using the native pub/sub functions and
    task executors. The idea is to have just one handler with a pool of workers subscribed
    to one channel. So all published messages to that channel will be processed only by one
    worker attached to the handler (since there is only one subscribed handler).


ErlBus with Riak Core and Gproc local
-------------------------------------

**ErlBus** is distributed by default, inherits all properties of [Distributed Erlang](http://www.erlang.org/doc/reference_manual/distributed.html)
and [`pg2`](http://erlang.org/doc/man/pg2.html). But `pg2` has some limitations, distribution model
works with full replication, which can cause problem when we have a considerable amount of subscribers,
and at the same time the amount of messages sent is too high. So for these scenarios **ErlBus** provides
another option: `ebus_dist`, which is built on top of `riak_core` and `gproc`.

To start `ebus_dist`, you must invoke `erl` with: `-ebus ebus_dist all`, and with a configuration file (with
`riak_core` config): `-config your_config_file.config` (you can use files located in `config/*.config`).

Let's start two Erlang nodes:

    $ erl -name node1@127.0.0.1 -setcookie ebus -pa ebin deps/*/ebin -ebus ebus_dist all -config config/dev1.config

    $ erl -name node2@127.0.0.1 -setcookie ebus -pa ebin deps/*/ebin -ebus ebus_dist all -config config/dev2.config

Start `ebus` on each one:

```erlang
% Start ebus
application:start(ebus).
12:05:30.633 [info] Application lager started on node 'node1@127.0.0.1'
12:05:30.743 [info] Application sasl started on node 'node1@127.0.0.1'
12:05:30.768 [info] Application crypto started on node 'node1@127.0.0.1'
12:05:30.801 [info] Application riak_sysmon started on node 'node1@127.0.0.1'
12:05:30.884 [info] Application os_mon started on node 'node1@127.0.0.1'
12:05:30.892 [info] alarm_handler: {set,{system_memory_high_watermark,[]}}
12:05:30.918 [info] Application basho_stats started on node 'node1@127.0.0.1'
12:05:30.938 [info] Application eleveldb started on node 'node1@127.0.0.1'
12:05:30.960 [info] Application pbkdf2 started on node 'node1@127.0.0.1'
12:05:30.981 [info] Application poolboy started on node 'node1@127.0.0.1'
12:05:31.086 [info] Starting reporters with []
12:05:31.086 [info] Application exometer_core started on node 'node1@127.0.0.1'
12:05:31.154 [info] Application clique started on node 'node1@127.0.0.1'
12:05:31.374 [warning] No ring file available.
12:05:31.607 [info] monitor long_schedule <0.160.0>  [{timeout,62},{in,{code_server,call,2}},{out,{code_server,'-handle_on_load/4-fun-0-',1}}]
12:05:32.306 [info] New capability: {riak_core,vnode_routing} = proxy
12:05:32.626 [info] New capability: {riak_core,staged_joins} = true
12:05:32.772 [info] New capability: {riak_core,resizable_ring} = true
12:05:32.895 [info] New capability: {riak_core,fold_req_version} = v2
12:05:33.039 [info] New capability: {riak_core,security} = true
12:05:33.196 [info] New capability: {riak_core,bucket_types} = true
12:05:33.330 [info] New capability: {riak_core,net_ticktime} = true
12:05:33.779 [info] Application riak_core started on node 'node1@127.0.0.1'
12:05:33.950 [info] Application ebus started on node 'node1@127.0.0.1'
ok
```

Add `node2` to cluster using node1:

```erlang
% From node2:
ebus_dist_console:join(["node1@127.0.0.1"]).
Sent join request to node1@127.0.0.1
ok
```

Now `node1` and `node2` are in cluster. From here is the same as previous examples.


Examples
--------

See [examples](./examples).


Running Tests
-------------

    $ make test


Change Log
----------

All notable changes to this project will be documented in the [CHANGELOG.md](CHANGELOG.md).
