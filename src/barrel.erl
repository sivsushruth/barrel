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

-export([all_databases/0, all_databases/2]).
-export([create_database/3]).
-export([open_database/1]).
-export([close_database/1]).
-export([delete_database/1]).

-type dbname() :: string().
-type all_databases_fun() :: fun((dbname(), any()) -> any()).
-type db() :: map().

-export_types([dbname/0]).
-export_types([all_databases_fun/0]).
-export_types([db/0]).

%% @doc list all databases
-spec all_databases() -> [barrel:dbname()].
all_databases() ->
  barrel_manager:all_databases().

%% @doc fold all databases names with a function
-spec all_databases(barrel:all_databases_fun(), any()) -> any().
all_databases(Fun, AccIn) ->
  barrel_manager:all_databases(Fun, AccIn).

%% @doc create a database
-spec create_database(atom(), dbname(), list()) -> {ok, db()} | {error, term()}.
create_database(Type, DbName, Options) ->
  barrel_manager:create_database(Type, DbName, Options).

%% @doc open a database
-spec open_database(dbname()) -> {ok, db()} | {error, term()}.
open_database(DbName) ->
  barrel_manager:create_database(DbName).

%% @doc close a database
-spec close_database(db()) -> ok | {error, term()}.
close_database(Db) ->
  barrel_manager:close_database(Db).

%% @doc delete a database
-spec delete_database(dbname()) -> ok.
delete_database(DbName) ->
  barrel_manager:delete_database(DbName).
