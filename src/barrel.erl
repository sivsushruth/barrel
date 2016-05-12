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

-export([all_databases/0, all_databases/2]).

-type db() :: atom().
-type all_databases_fun() :: fun((db(), any()) -> any()).


-export_types([db/0]).
-export_types([all_databases_fun/0]).


%% @doc list all databases
-spec all_databases() -> [barrel:db()].
all_databases() ->
  barrel_manager:all_databases().

%% @doc fold all databases names with a function
-spec all_databases(barrel:all_databases_fun(), any()) -> any().
all_databases(Fun, AccIn) ->
  barrel_manager:all_databases(Fun, AccIn).
