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


-module(barrel_rocksdb_test).

-compile([export_all/1]).
-include_lib("eunit/include/eunit.hrl").


test_dir() ->
  filename:absname(lists:concat(["barrel_tests", ".", node()])).

setup() ->
  TestDir = test_dir() ,
  application:set_env(barrel, data_dir, TestDir),
  os:cmd("rm -rf " ++ TestDir),
  {ok, _} = application:ensure_all_started(barrel).


simple_database_test() ->
  setup(),
  {ok, Db} = barrel:create_database(rocksdb_disc, "testdb", []),
  ?assert(filelib:is_dir(filename:join([test_dir(), "testdb"]))),
  ?assertEqual(["testdb"], barrel:all_databases()),
  ?assertEqual(ok, barrel:close_database(Db)),
  ?assertEqual(["testdb"], barrel:all_databases()),
  ?assertEqual(ok, barrel:delete_database("testdb")),
	io:format("dir is ~p~n", [file:list_dir_all(barrel_manager:db_dir("testdb"))]),
  ?assertEqual(false, filelib:is_dir(filename:join([test_dir(), "testdb"]))),

  ?assertEqual([], barrel:all_databases()),
  ok.


database_test() ->
  setup(),
  {ok, Db} = barrel:create_database(rocksdb_disc, "testdb", []),
  ?assert(filelib:is_dir(filename:join([test_dir(), "testdb"]))),
  ?assertEqual(["testdb"], barrel:all_databases()),
  {ok, Db1} = barrel:create_database(rocksdb_disc, "testdb1", []),
  ?assert(filelib:is_dir(filename:join([test_dir(), "testdb1"]))),
  {ok, Db2} = barrel:create_database(rocksdb_ram, "testdb2", []),
  ?assertEqual(true, filelib:is_dir(filename:join([test_dir(), "testdb2"]))),
  ?assertEqual(["testdb", "testdb1", "testdb2"], barrel:all_databases()),

  Fun = fun(DbName, Acc) ->
            {ok, Alias, _Options} = barrel_manager:load_latest_options(DbName),
            [{DbName, Alias} | Acc]
        end,

  All = barrel:all_databases(Fun, []),

  ?assertEqual([{"testdb", rocksdb_disc},
                {"testdb1", rocksdb_disc},
                {"testdb2", rocksdb_ram}], lists:ukeysort(1, All)),


  barrel:close_database(Db),
  barrel:close_database(Db1),
  barrel:close_database(Db2),


  ok.
