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

%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2016 18:57
%%%-------------------------------------------------------------------
-module(barrel_rocksdb).
-author("benoitc").

%% API
-export([init/1, init_memdb/1]).
-export([close/1]).
-export([capacity/1]).
-export([put/3, get/2, delete/2, iterator/1]).

-export([snapshot/1]).
-export([close_snapshot/1]).
-export([snapshot_get/2, snapshot_iterator/1]).

-record(rocksdb, {db,
                  dir,
                  capacity}).

-record(rocksdb_snapshot, {db, snapshot = nil, parent}).

init(Config) ->
  Dir = proplists:get_value(path, Config, undefined),
  MaxCapacity= barrel_lib:get_disk_size(Dir),
  Size = proplists:get_value(size, Config, MaxCapacity),
  Capacity = barrel_lib:calculate_size(Size, MaxCapacity),
  case erocksdb:open(Dir, [{create_if_missing, true}]) of
    {ok, Db} ->
      {ok, {?MODULE, #rocksdb{db=Db, dir= Dir, capacity = Capacity}}};
    Error ->
      Error
  end.

init_memdb(Config) ->
  MaxCapacity = barrel_lib:get_ram_size(),
  Size = proplists:get_value(size, Config, MaxCapacity),
  Capacity = barrel_lib:calculate_size(Size, MaxCapacity),
  case erocksdb:open("", [{create_if_missing, true}]) of
    {ok, Db} ->
      {ok, {?MODULE, #rocksdb{db=Db, dir="", capacity = Capacity}}};
    Error ->
      Error
  end.

capacity(#rocksdb{capacity=Capacity}) ->
  Capacity.


close(#rocksdb{db=Db}) ->
  erocksdb:close(Db).


put(#rocksdb{db=Db}, Key, Value) ->
  erocksdb:put(Db, Key, Value, []).

get(#rocksdb{db=Db}, Key) ->
  erocksdb:get(Db, Key, []).

delete(#rocksdb{db=Db}, Key) ->
  erocksdb:delete(Db, Key, []).

iterator(#rocksdb{db=Db}) ->
  erocksdb:iterator(Db, []).

snapshot(#rocksdb{db=Db}=RocksDB) ->
  {ok, Snapshot} = erocksdb:snapshot(Db),
  #rocksdb_snapshot{db=Db, snapshot=Snapshot, parent=RocksDB}.

close_snapshot(#rocksdb_snapshot{snapshot=S}) ->
  erocksdb:release_snapshot(S).

snapshot_get(#rocksdb_snapshot{db=Db, snapshot=S}, Key) ->
  erocksdb:get(Db, Key, [{snapshot, S}]).

snapshot_iterator(#rocksdb_snapshot{db=Db, snapshot=S}) ->
  erocksdb:iterator(Db, [{snapshot, S}]).
