<img src="http://assets.nydailynews.com/polopoly_fs/1.1096240.1339765703!/img/httpImage/image.jpg_gen/derivatives/article_970/trains15n-1-web.jpg" height="200" width="100%" />

# ErlBus

Message / Event Bus written in Erlang.

The PubSub core is a clone of the original, remarkable, and proven [Phoenix PubSub Layer](https://hexdocs.pm/phoenix/Phoenix.PubSub.html),
but re-written in Erlang.

A new way to build soft real-time and high scalable messaging-based applications, not centralized but distributed!

See also: [WEST](https://github.com/cabol/west).


## Introduction

**ErlBus** is a simple and lightweight library/tool to build messaging-based applications.

**ErlBus** PubSub implementation was taken from [Phoenix Framework](http://www.phoenixframework.org/),
which provides an amazing, scalable and proven PubSub solution. In addition to this,
**ErlBus** provides an usable and simpler interface on top of this implementation.

You can read more about the PubSub implementation [HERE](https://hexdocs.pm/phoenix/Phoenix.PubSub.html).


## Building ErlBus

Assuming you have a working Erlang installation (recommended 17 or later), building **ErlBus** should be as simple as:

    $ git clone https://github.com/cabol/erlbus.git
    $ cd erlbus
    $ make


## Quick Start Example

Start an Erlang console with `ebus` running:

    $ make shell

Once into the erlang console:

```erlang
% subscribe the current shell process
ebus:sub(self(), "foo").
ok

% spawn a process
Pid = spawn_link(fun() -> timer:sleep(infinity) end).
<0.57.0>

% subscribe spawned PID
ebus:sub(Pid, "foo").
ok

% publish a message
ebus:pub("foo", {foo, "hi"}).
ok

% check received message for Pid
ebus_process:messages(Pid).             
[{foo,"hi"}]

% check received message for self
ebus_process:messages(self()).             
[{foo,"hi"}]

% unsubscribe self
ebus:unsub(self(), "foo").
ok

% publish other message
ebus:pub("foo", {foo, "hello"}).
ok

% check received message for Pid
ebus_process:messages(Pid).             
[{foo,"hi"},{foo,"hello"}]

% check received message for self (last message didn't arrive)
ebus_process:messages(self()).             
[{foo,"hi"}]

% check subscribers (only Pid should be in the returned list)
ebus:subscribers("foo").
[<0.57.0>]

% check topics
ebus:topics().
[<<"foo">>]

% subscribe self to other topic
ebus:sub(self(), "bar").
ok

% check topics
ebus:topics().
[<<"bar">>,<<"foo">>]

% publish other message
ebus:pub("bar", {bar, "hi bar"}).
ok

% check received message for Pid (last message didn't arrive)
ebus_process:messages(Pid).             
[{foo,"hi"},{foo,"hello"}]

% check received message for self
ebus_process:messages(self()).             
[{foo,"hi"},{bar,"hi bar"}]
```

> **Note:**

> - You may have noticed that is not necessary additional steps/calls to create/delete a topic,
    this is automatically handled by `ebus`, so you don't worry about it!

Now, let's make it more fun, start two Erlang consoles, first one:

    $ erl -name node1@127.0.0.1 -setcookie ebus -pa _build/default/lib/*/ebin -s ebus -config test/test.config

The second one:

    $ erl -name node2@127.0.0.1 -setcookie ebus -pa _build/default/lib/*/ebin -s ebus -config test/test.config

Then what we need to do is put these Erlang nodes in cluster, so from any of them send a ping
to the other:

```erlang
% From node1 ping node2
net_adm:ping('node2@127.0.0.1').
pong
```

Excellent, we have both nodes in cluster, thanks to the beauty of [Distributed Erlang](http://www.erlang.org/doc/reference_manual/distributed.html).
So, let's repeat the above exercise but now in two nodes.

In the `node1` create a handler and subscription to some topic:

```erlang
% create a callback fun to use ebus_process utility
CB1 = fun(Msg) ->
  io:format("CB1: ~p~n", [Msg])
end
#Fun<erl_eval.6.54118792>

% other callback but receiving an initial context,
% that may be used when message arrives
CB2 = fun(Ctx, Msg) ->
  io:format("CB2: Ctx: ~p, Msg: ~p~n", [Ctx, Msg])
end.
#Fun<erl_eval.12.54118792>

% use ebus_process utility to spawn a handler
H1 = ebus_process:spawn_handler(CB1).
<0.69.0>
H2 = ebus_process:spawn_handler(CB2, "any_ctx").
<0.80.0>

% subscribe handlers
ebus:sub(H1, "foo").
ok
ebus:sub(H2, "foo").
ok
```

Repeat the same thing above in `node2`.

Once you have handlers subscribed to the same channel in both nodes, publish some messages from
any node:

```erlang
% publish message
ebus:pub("foo", {foo, "again"}).
CB2: Ctx: "any_ctx", Msg: {foo,"again"}
CB1: {foo,"again"}
ok
```

And in the other node you will see those messages have arrived too:

```erlang
CB1: {foo,"again"}  
CB2: Ctx: "any_ctx", Msg: {foo,"again"}
```

Let's check subscribers, so from any Erlang console:

```erlang
% returns local and remote subscribers
ebus:subscribers("foo").
[<7014.66.0>,<0.69.0>]
```

You can also check the [TESTS](./test/) for more info about to use `ebus`.

So far, so good! Let's continue!


## Point-To-Point Example

The great thing here is that you don't need something special to implement a point-to-point behavior.
It is as simple as this:

```erlang
ebus:dispatch("topic1", #{payload => "M1"}).
```

Dispatch function gets the subscribers and then picks one of them to send the message out.
You can provide a dispatch function to pick up a subscriber, otherwise, a default function
is provided (picks a subscriber random).

Dispatch function comes in 3 different flavors:

 * `ebus:dispatch/2`: receives the topic and the message.
 * `ebus:dispatch/3`: receives the topic, message and a list of options.
 * `ebus:dispatch/4`: same as previous but receives as 1st argument the name of the server,
   which is placed by default in the other functions.

Dispatch options are:

 * `{scope, local | global}`: allows you to choose if you want to pick a local subscriber o any.
   Default value: `local`.
 * `{dispatch_fun, fun(([term()]) -> term())}`: function to pick up a subscriber.
   If it isn't provided, a default random function is provided.

To see how this function is implemented go [HERE](./src/ebus.erl).

Let's see an example:

```erlang
% subscribe local process
ebus:sub(self(), "foo").
ok

% spawn a process
Pid = spawn_link(fun() -> timer:sleep(infinity) end).
<0.57.0>

% subscribe spawned PID
ebus:sub(Pid, "foo").
ok

% check that we have two subscribers
ebus:subscribers("foo").
[<0.57.0>,<0.38.0>]

% now dispatch a message (default dispatch fun and scope)
ebus:dispatch("foo", #{payload => foo}).
ok

% check that only one subscriber received the message
ebus_process:messages(self()).
[#{payload => foo}]
ebus_process:messages(Pid).
[]

% dispatch with options
Fun = fun([H | _]) -> H end.
#Fun<erl_eval.6.54118792>
ebus:dispatch("foo", <<"M1">>, [{scope, global}, {dispatch_fun, Fun}]).
ok

% check again
ebus_process:messages(self()).                                         
[#{payload => foo}]
ebus_process:messages(Pid).                                            
[<<"M1">>]
```

Extremely easy isn't?

## Distributed ErlBus

**ErlBus** is distributed by nature, it doesn't require any additional/magical thing.

Once you have an Erlang cluster, messages are broadcasted using [PG2](http://erlang.org/doc/man/pg2.html),
which is the default PubSub adapter. Remember, it's a [Phoenix PubSub](https://hexdocs.pm/phoenix/Phoenix.PubSub.html) clone,
so the architecture and design it's the same.

[Phoenix Channels](http://www.phoenixframework.org/docs/channels) are supported on PubSub layer,
which is the core. Take a look at this [blog post](http://www.phoenixframework.org/blog/the-road-to-2-million-websocket-connections).


## Examples

See [examples](./examples).


## Running Tests

    $ make tests


## Change Log

All notable changes to this project will be documented in the [CHANGELOG.md](CHANGELOG.md).


## Copyright and License

Original work Copyright (c) 2014 Chris McCord

Modified work Copyright (c) 2016 Carlos Andres Bolaños

**ErlBus** source code is licensed under the [MIT License](LICENSE.md).

> **NOTE:**: Pub/Sub implementation was taken entirely from [Phoenix Framework](https://github.com/phoenixframework/phoenix).
