
ErlBus
======

Message / Event Bus written in Erlang.

__Authors:__ Carlos Andres Bolaños R.A. ([`candres@niagara.io`](mailto:candres@niagara.io)).

A new way to build soft real-time and high scalable messaging-based applications, not centralized but distributed!

See also: [WEST](https://github.com/cabol/west).


Building ErlBus
---------------

Assuming you have a working Erlang installation (recommended 17 or later), building **Erlbus** should be as simple as:

    $ git clone https://github.com/cabol/erlbus.git
    $ cd erlbus
    $ make


Note
----

**ErlBus** has 3 dependencies: `gproc`, `poolboy` and `riak_core`. But the last one `riak_core` is fetched
on-demand, when you want to run **ErlBus** in distributed mode, inheriting all `riak_core` benefits.
Learn more about [Riak Core](https://github.com/basho/riak_core). In this scenario, **ErlBus** runs
with `gproc` locally on each node, and `riak_core` on top of it, managing the cluster and task/command
distribution.

- To enable fetching of `riak_core`, export the OS environment variable `EBUS_DIST=true` (this can be done
  e.g. from a GNU Makefile).


Introduction
------------

**ErlBus** is a very lightweight and simple approach to build messaging-based apps. Messaging infrastructure
that provides: Publish/Subscribe, Point-To-Point, Message Handler, Event Driven Consumer, Task Executor, etc.


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
F = fun(Topic, Msg) -> io:format("Topic: ~p - Msg: ~p~n", [Topic, Msg]) end.
#Fun<erl_eval.12.90072148>

% Create anonymous handlers
MH1 = ebus_handler:new_anonymous(F).
<0.50.0>
MH2 = ebus_handler:new_anonymous(F).
<0.52.0>

% Subscribe tw0 of them to topic t1
ebus:sub(t1, MH1).
ok
ebus:sub(t1, MH2).
ok

% Let's publish a message to 't1'
ebus:pub(t1, "Hello!").
Topic: t1 - Msg: "Hello!"
Topic: t1 - Msg: "Hello!"
ok

% Another handler
F2 = fun(Topic, Msg) -> io:format("OTHER -- Topic: ~p - Msg: ~p~n", [Topic, Msg]) end.
#Fun<erl_eval.12.90072148>
MH3 = ebus_handler:new_anonymous(F2).
<0.54.0>

% Subscribe the other handler 'MH3' to t2
ebus:sub(t2, MH3).
ok

% Publish to 't2'
ebus:pub(t2, "Hello other!").
OTHER -- Topic: t2 - Msg: "Hello other!"
ok

% Unsubscribe 'MH2' from t1
ebus:unsub(t1, MH2).
ok

% Publish again to 't1'
ebus:pub(t1, "Hello again!").
Topic: t1 - Msg: "Hello again!"
ok
```

Now, let's make it more fun, start two Erlang consoles, first one:

    $ erl -name node1@127.0.0.1 -setcookie ebus -pa ebin deps/*/ebin

The second one:

    $ erl -name node1@127.0.0.1 -setcookie ebus -pa ebin deps/*/ebin

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

Then in `node1` create a handler with subscribe it to a topic:

```erlang
% Anonymous handler function
F = fun(Topic, Msg) -> io:format("Topic: ~p - Msg: ~p~n", [Topic, Msg]) end.
#Fun<erl_eval.12.90072148>

% Subscribe a handler
ebus:sub(t1, ebus_handler:new_anonymous(F)).
ok
```

Repeat the same thing above in `node2`.

Once you have handlers subscribed to the same topic in both nodes, publish some messages from
any node:

```erlang
% Publish message
ebus:pub(t1, "Hi!").
Topic: t1 - Msg: "Hi!"
ok
```

And in the other node you will see that message has arrived too:

```erlang
Topic: t1 - Msg: "Hi!"
ok
```

So far, so good! Let's continue!


Basic Pub/Sub Example
---------------------

Previously we say a simple example how Pub/Sub works, but using anonymous handlers, which aren't bad
in case of any demo o simple test. But the right way would be create your own message handler, using
the `ebus_handler` beahvior. Because in this way, your handler will be part of the supervision tree,
and you will be able to use other features too, that we'll cover later.

### my_handler.erl

```erlang
-module(my_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({Topic, Msg}, Context) ->
  io:format("[Pid: ~p][Topic: ~p][Msg: ~p][Ctx: ~p]~n",
            [self(), Topic, Msg, Context]).
```

Once you have compiled your module(s) and started an Erlang console:

```erlang
% Start ebus
application:start(ebus).
ok

% Create a new handler
MH1 = ebus_handler:new(my_handler).
<0.49.0>

% From here, everything is the same as previous example
% Subscribe the handler to some topic
ebus:sub(my_topic, MH1).
ok

% Now the handler is ready to receive and process messages
% Publish a message/event
ebus:pub(my_topic, "Hello!").
ok
```

Again, you can start multiple Erlang nodes, put them in cluster, start `ebus` on each one,
and now you have `ebus` running in distributed fashion, it's extremely easy, you have not to do
anything at all.


ErlBus with Riak Core and Gproc local
-------------------------------------

**ErlBus** is distributed by default, inherits all properties of Distributed Erlang and [PG2](http://erlang.org/doc/man/pg2.html).
But `pg2` has some limitations, distribution model is with full replication, which can cause problem when
we have a considerable amount of subscribers, and at the same time the amount of messages sent is too high.
So for these scenarios **ErlBus** provides another option: `ebus_dist`, which is built on top of `riak_core`
and `gproc`.

To start `ebus_dist`, you must invoke `erl` with: `-ebus ebus_dist all`, and with a configuration file (with
`riak_core` config): `-config your_config_file.config` (you can use files located in `config/*.config`).

Let's start two Erlang nodes:

    $ erl -name node1@127.0.0.1 -setcookie ebus -pa ebin deps/*/ebin -ebus ebus_dist all -config config/dev1.config

    $ erl -name node2@127.0.0.1 -setcookie ebus -pa ebin deps/*/ebin -ebus ebus_dist all -config config/dev2.config

Start `ebus` on each one:

```erlang
% Start ebus
application:start(ebus).
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


Running Tests
-------------

    % make test


Building Edoc
-------------

No supported yet.
