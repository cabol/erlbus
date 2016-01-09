%%%-------------------------------------------------------------------
%%% @doc
%%% This is an Erlang clone of the original
%%% `Phoenix.Transports.Serializer' module.
%%% Copyright (c) 2014 Chris McCord
%%% @reference See
%%% <a href="https://github.com/phoenixframework/phoenix">Phoenix</a>
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_serializer).

%% Translates a `ebus_broadcast:broadcast()' to fastlane format
-callback fastlane(ebus_broadcast:broadcast()) -> term().

%% Encodes `ebus_broadcast:message()' to transport representation
-callback encode(ebus_broadcast:message() | ebus_broadcast:reply()) -> term().

%% Decodes data into `ebus_broadcast:message()' spec
-callback decode(binary(), [{atom(), term()}]) -> ebus_broadcast:message().
