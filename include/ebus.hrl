%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions and macros.
%%%
%%% Messages were taken from the originals:
%%% * `Phoenix.Socket.Message`
%%% * `Phoenix.Socket.Reply`
%%% * `Phoenix.Socket.Broadcast`
%%%
%%% @see <a href="https://github.com/phoenixframework/phoenix"></a>
%%% @end
%%%-------------------------------------------------------------------

%%%===================================================================
%%% Message specs
%%%===================================================================

%% Message Type
%%
%% Defines a message dispatched over transport to channels and vice-versa.
%%
%% The message format requires the following keys:
%%
%% * `topic`: The string topic or `topic:subtopic` pair namespace,
%%   for example <<"messages">>, <<"messages:123">>
%% * `event`: The string event name, for example <<"phx_join">>
%% * `payload`: The message payload
%% * `ref`: The unique string ref
-record(message, {
  topic   = nil :: binary() | nil,
  event   = nil :: binary() | nil,
  payload = nil :: term(),
  ref     = nil :: binary() | nil
}).

-type message() :: #message{}.

%% Reply Type
%%
%% Defines a reply sent from channels to transports.
%%
%% The message format requires the following keys:
%%
%% * `topic`: The string topic or `topic:subtopic` pair namespace,
%%   for example <<"messages">>, <<"messages:123">>
%% * `status`: The reply status as an atom
%% * `payload`: The message payload
%% * `ref`: The unique string ref
-record(reply, {
  topic   = nil :: binary() | nil,
  status  = nil :: atom(),
  payload = nil :: term(),
  ref     = nil :: binary() | nil
}).

-type reply() :: #reply{}.

%% Broadcast Type
%%
%% Defines a message sent from pubsub to channels and vice-versa.
%%
%% The message format requires the following keys:
%%
%% * `topic`: The string topic or `topic:subtopic` pair namespace,
%%   for example <<"messages">>, <<"messages:123">>
%% * `event`: The string event name, for example <<"phx_join">>
%% * `payload`: The message payload
-record(broadcast, {
  topic   = nil :: binary() | nil,
  event   = nil :: binary() | nil,
  payload = nil :: term()
}).

-type broadcast() :: #broadcast{}.

%%%===================================================================
%%% Macros
%%%===================================================================

%% This macro will create a function that converts a record to
%% a {key, value} list (a proplist)
%% Taken from <a href="https://gist.github.com/gdamjan/1272771"/>
-define(record_to_proplist(Record),
  fun(Val) ->
    Fields = record_info(fields, Record),
    [_Tag | Values] = tuple_to_list(Val),
    lists:zip(Fields, Values)
  end
).

%% This is a clever Erlang macro that given a record name will create
%% a new function that converts from a property list to the record
%% (which really is a tagged tuple).
-define(proplist_to_record(Record, Tuple),
  fun(Proplist) ->
    Fields = record_info(fields, Record),
    [Tag | Values] = tuple_to_list(Tuple),
    Defaults = lists:zip(Fields, Values),
    F = fun({K, V}) ->
          case lists:keyfind(K, 1, Proplist) of
            {_K, V0} -> V0;
            _        -> V
          end
        end,
    L = lists:map(F, Defaults),
    list_to_tuple([Tag | L])
  end
).
