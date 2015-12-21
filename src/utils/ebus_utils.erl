%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Carlos Andres Bolaños
%%% @doc
%%% Common utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(ebus_utils).

%% API
-export([keyfind/2, keyfind/3, rand_elem/1]).
-export([build_name/1, build_name/2]).
-export([to_bin/1, to_atom/1, to_int/1, to_float/1, to_list/1]).
-export([pmap/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Calls keyfind/3 with Default = undefined.
-spec keyfind(any(), term()) -> term().
keyfind(Key, TupleList) ->
  keyfind(Key, TupleList, undefined).

%% @doc Searches the list of tuples TupleList for a tuple whose Nth element
%%      compares equal to Key. Returns Tuple's value if such a tuple is
%%      found, otherwise Default.
-spec keyfind(any(), term(), any()) -> term().
keyfind(Key, TupleList, Default) ->
  case lists:keyfind(Key, 1, TupleList) of
    {_K, V} -> V;
    _       -> Default
  end.

%% @doc Returns a random element from given list `L'.
-spec rand_elem([term()]) -> term().
rand_elem(L) when is_list(L), length(L) > 0 ->
  N = (erlang:phash2(os:timestamp()) rem length(L)) + 1,
  lists:nth(N, L).

%% @equiv build_name(L, <<"_">>)
build_name(L) ->
  build_name(L, <<"_">>).

%% @doc Build a name given the list of terms, then they are transformed
%%      to binary and concatenated by `Separator'.
-spec build_name([any()], iodata()) -> atom().
build_name(L, Separator) when is_list(L) ->
  Fun = fun
    (X, <<"">>) ->
      <<(to_bin(X))/binary>>;
    (X, Acc) ->
      <<Acc/binary, (to_bin(Separator))/binary, (to_bin(X))/binary>>
  end,
  binary_to_atom(lists:foldl(Fun, <<"">>, L), utf8).

%% @doc Converts any type to binary.
-spec to_bin(any()) -> binary().
to_bin(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_bin(Data) when is_float(Data) ->
  float_to_binary(Data);
to_bin(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_bin(Data) when is_list(Data) ->
  iolist_to_binary(Data);
to_bin(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_binary(erlang:phash2(Data));
to_bin(Data) ->
  Data.

%% @doc Converts any type to atom.
-spec to_atom(any()) -> atom().
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
  Data.

%% @doc Converts any type to integer.
-spec to_int(any()) -> integer().
to_int(Data) when is_binary(Data) ->
  binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
  list_to_integer(Data);
to_int(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_int(Data) ->
  Data.

%% @doc Converts any type to float.
-spec to_float(any()) -> float().
to_float(Data) when is_binary(Data) ->
  binary_to_float(Data);
to_float(Data) when is_list(Data) ->
  list_to_float(Data);
to_float(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_float(Data) ->
  Data.

%% @doc Converts any type to list.
-spec to_list(any()) -> list().
to_list(Data) when is_binary(Data) ->
  binary_to_list(Data);
to_list(Data) when is_integer(Data) ->
  integer_to_list(Data);
to_list(Data) when is_float(Data) ->
  float_to_list(Data);
to_list(Data) when is_atom(Data) ->
  atom_to_list(Data);
to_list(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_list(erlang:phash2(Data));
to_list(Data) ->
  Data.

%% @doc applies F to each element of L in a separate process. The results
%%      returned may not be in the same order as they appear in L.
pmap(F, L) ->
  S = self(),
  Ref = erlang:make_ref(),
  lists:foreach(
    fun(I) ->
      spawn_link(fun() -> do_f(S, Ref, F, I) end)
    end, L
  ),
  gather(length(L), Ref, []).

%% @private
do_f(Parent, Ref, F, I) ->
  Parent ! {Ref, (catch F(I))}.

%% @private
gather(0, _, L) -> L;
gather(N, Ref, L) ->
  receive
    {Ref, Ret} -> gather(N - 1, Ref, [Ret | L])
  end.
