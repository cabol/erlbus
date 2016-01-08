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

-include("ebus.hrl").

%% Translates a `broadcast()' to fastlane format
-callback fastlane(broadcast()) -> term().

%% Encodes `message()' to transport representation
-callback encode(message() | reply()) -> term().

%% Decodes data into `message()' spec
-callback decode(binary(), Opts :: [{atom(), term()}]) -> message().
