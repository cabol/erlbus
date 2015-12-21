%% -------------------------------------------------------------------
%% Copyright (c) 2014 Chris McCord
%% -------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @doc
%%% This is an Erlang clone of the original
%%% `Phoenix.Transports.Serializer` module.
%%% @see <a href="https://github.com/phoenixframework/phoenix"></a>
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_serializer).

-include("ebus.hrl").

%% @doc Translates a `broadcast()` to fastlane format
-callback fastlane(broadcast()) -> term().

%% @doc Encodes `message()` to transport representation
-callback encode(message() | reply()) -> term().

%% @doc Decodes data into `message()` spec
-callback decode(binary(), Opts :: [{atom(), term()}]) -> message().
