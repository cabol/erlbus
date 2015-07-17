%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Carlos Andres Bolaños, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2015, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc Common utilities.
%%%-------------------------------------------------------------------
-module(ebus_util).

%% API
-export([keyfind/2, keyfind/3, rem_dups_from_list/1, count_val_in_list/2,
         to_bin/1, to_atom/1, to_integer/1, to_float/1, to_list/1,
         build_name/1, get_best_pid/1]).

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

%% @doc Removes duplicates from the given list and return a new filtered list.
-spec rem_dups_from_list([term()]) -> [term()].
rem_dups_from_list(L) ->
  F = fun(E, Acc) ->
        case lists:member(E, Acc) of
          true  -> Acc;
          false -> [E | Acc]
        end
      end,
  lists:foldl(F, [], L).

%% @doc Count the number of occurrences of 'Val' in the list 'L'.
-spec count_val_in_list(any(), [term()]) -> non_neg_integer().
count_val_in_list(Val, L) ->
  F = fun(E, Acc) ->
        case E =:= Val of
          true  -> Acc + 1;
          false -> Acc
        end
      end,
  lists:foldl(F, 0, L).

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
-spec to_integer(any()) -> integer().
to_integer(Data) when is_binary(Data) ->
  binary_to_integer(Data);
to_integer(Data) when is_list(Data) ->
  list_to_integer(Data);
to_integer(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_integer(Data) ->
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

%% @doc Build a name given the list of terms, then they are transformed
%%      to binary and concatenated by '_'.
-spec build_name([any()]) -> atom().
build_name(L) when is_list(L) ->
  F = fun(X, <<"">>) ->
        <<(to_bin(X))/binary>>;
      (X, Acc) ->
        <<Acc/binary, (<<"_">>)/binary, (to_bin(X))/binary>>
      end,
  binary_to_atom(lists:foldl(F, <<"">>, L), utf8).

%% @doc It simply iterates through the process list and choose the process
%%      with the shortest queue.
-spec get_best_pid([pid()]) -> pid().
get_best_pid(Pids) ->
  F = fun(Pid, Acc) ->
        Key = message_queue_len,
        [{Key, Messages}] = erlang:process_info(Pid, [Key]),
        [{Pid, Messages} | Acc]
      end,
  Members = lists:foldl(F, [], Pids),
  case lists:keysort(2, Members) of
    [{Pid, _} | _] ->
      Pid;
    [] ->
      {error, empty_list}
  end.
