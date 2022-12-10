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

%% Translates a `ebus_broadcast:t()' to fastlane format
-callback fastlane(ebus_broadcast:t()) -> term().

%% Encodes `ebus_broadcast:t()' to transport representation
-callback encode(ebus_message:t() | ebus_reply:t()) -> term().

%% Decodes data into `ebus_broadcast:t()' spec
-callback decode(binary(), [{atom(), term()}]) -> ebus_message:t().
