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

-module(barrel).

-export([open/2]).
-export([close/1]).
-export([await/1]).

-type backend() :: atom().
-type db() :: atom() | string() | binary().

-type open_options() :: [{backend, backend()} |
                         {file, string() | binary()} |
                         {db_opts, list()}].

-export_types([backend/0, db/0, open_options/0]).


%% @doc open a database
-spec open(db(), open_options()) -> {ok, pid()} | {error, any()}.
open(DbName, Options) ->
  Backend = proplists:get_value(backend, Options, barrel_rocksdb),
  _ = code:ensure_loaded(Backend),
  case erlang:function_exported(Backend, open, 2) of
    false -> {error, badarg};
    true ->
      io:format("ici", []),
      case barrel_dbs_sup:start_db(Backend, DbName, Options) of
        {ok, _Pid} ->
           {ok, gproc:where({n,l,{barrel_db,DbName}})};
        Error ->
          Error
      end
  end.

%% @doc close a database
-spec close(db()) -> ok.
close(DbName) ->
  barrel_dbs_sup:stop_db(DbName).


%% @equiv await(DbName, 60000)
-spec await(db()) -> ok.
await(DbName) ->
  barrel_db:await(DbName).

%% @doc Waits for the barrel database `DbName' to become available.
await(DbName, Timeout) ->
  barrel_db:await(DbName, Timeout).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
  {ok, _} = application:ensure_all_started(barrel),
  Db = testdb,
  {ok, Pid} = barrel:open(Db, [{backend, barrel_rocksdb}]),
  io:format("pid is ~p~n", [Pid]),
  ?assert(is_pid(Pid) =:= true),
  ?assertEqual(ok, barrel:await(Db)),
  ?assert(filelib:is_dir("testdb") =:= true),
  ok = barrel:close(Db),
  barrel_os_util:rm_rf("testdb"),
  ok.

-endif.
