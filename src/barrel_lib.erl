%% Copyright 2016 Benoit Chesneau
%%
%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%% You may not use this work except in compliance with the Licence.
%% You may obtain a copy of the Licence at:
%%
%% https://joinup.ec.europa.eu/software/page/eupl
%%
%% Unless required by applicable law or agreed to in  writing, software
%% distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the Licence for the specific language governing permissions and
%% limitations under the Licence.

-module(barrel_lib).

-export([val/1, val/2]).
-export([set/2]).
-export([unset/1]).

-export([db_file/1]).
-export([load_config/2]).

-export([to_binary/1]).
-export([to_list/1]).
-export([new_id/1]).
-export([take_last/1]).
-export([timestamp/0]).
-export([to_hex/1]).

-include_lib("syntax_tools/include/merl.hrl").
val(Key) ->
  val(Key, undefined).

val(Key, Default) ->
  try ets:lookup_element(barrel_gvar, Key, 2)
  catch error:_ -> Default
  end.

set(Key, Val) ->
  ets:insert(barrel_gvar, {Key, Val}).

unset(Key) ->
  ets:delete(barrel_gvar, Key).


-spec db_file(any()) -> list().
db_file(Name) ->
  File = to_list(Name),
  filelib:ensure_dir(File),
  File.

to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_list(V) -> V.

to_binary(S) when is_list(S) ->
  list_to_binary(S);
to_binary(A) when is_atom(A) ->
  list_to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) ->
  B.

new_id(string)    -> uuid:uuid_to_string(uuid:get_v4(), standard);
new_id(binary)    -> uuid:uuid_to_string(uuid:get_v4(), binary_standard);
new_id(text)      -> uuid:uuid_to_string(uuid:get_v4(), binary_nodash);
new_id(integer)   -> <<Id:128>> = uuid:get_v4(), Id;
new_id(float)     -> <<Id:128>> = uuid:get_v4(), Id * 1.0;
new_id(FieldType) -> throw({unimplemented, FieldType}).

%% @doc Utility that converts a given property list into a module that provides
%% constant time access to the various key/value pairs.
%%
%% Example:
%%
%%   load_config(store_config, [{backends, [{rocksdb_ram, barrel_rocksdb},
%%                                          {rocksdb_disk, barrel_rocksdb}]},
%%                              {data_dir, "/path/to_datadir"}]).
%%
%% creates the module store_config:
%%   store_config:backends(). => [{rocksdb_ram,barrel_rocksdb},{rocksdb_disk,barrel_rocksdb}]
%%   store_config:data_dir => "/path/to_datadir"
%%
-spec load_config(atom(), [{atom(), any()}]) -> ok.
load_config(Resource, Config) when is_atom(Resource), is_list(Config) ->
  Module = ?Q("-module(" ++ atom_to_list(Resource) ++ ")."),
  Functions = lists:foldl(fun({K, V}, Acc) ->
                              [make_function(K,
                                             V)
                               | Acc]
                          end,
                          [], Config),
  Exported = [?Q("-export([" ++ atom_to_list(K) ++ "/0]).") || {K, _V} <-
                                                               Config],
  Forms = lists:flatten([Module, Exported, Functions]),
  merl:compile_and_load(Forms, [verbose]),
  ok.

make_function(K, V) ->
    Cs = [?Q("() -> _@V@")],
      F = erl_syntax:function(merl:term(K), Cs),
        ?Q("'@_F'() -> [].").


%% @doc unique timestamp
-spec timestamp() -> {integer(), integer()}.
timestamp() ->
  T = erlang:monotonic_time(micro_seconds),
  U = erlang:unique_integer(),
  {T, U}.


%% @doc take the last item of a list and return a new list without it
-spec take_last(list()) -> {term(), list()}.
take_last([E]) ->
  {E, []};
take_last([E | Es]) ->
  take_last(E, Es, [E]).

take_last(_, [E], L) -> {E, lists:reverse(L)};
take_last(_, [E|Es], L) -> take_last(E, Es, [E | L]).

%% @doc convert a binary integer list to an hexadecimal
-spec to_hex(binary()) -> binary().
to_hex(Str) ->
    to_hex(Str, <<>>).

to_hex(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    HiChar = integer_to_hex_char(Hi),
    LoChar = integer_to_hex_char(Lo),
    to_hex(Tail, <<Acc/binary, HiChar, LoChar>>);
to_hex(<<>>, Acc) ->
    Acc.


% ====================================
% = Internal functions
% ====================================

integer_to_hex_char(N) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $a - 10 + N;
        true ->
            erlang:error(badarg)
    end.
