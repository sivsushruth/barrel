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


-module(barrel_controller).

-export([register_db/2]).
-export([unregister_db/1]).
-export([await/1, await/2]).
-export([where/1]).
-export([start_link/3]).

register_db(DbName, Module) ->
  gproc:reg({n, l, {barrel_db, DbName}}, Module).

unregister_db(DbName) ->
  gproc:unreg({n, l, {barrel_db, DbName}}).


await(Db) ->
  await(Db, timer:minutes(1)).

await(Db, Timeout) ->
  gproc:await({n, l, {barrel_db, Db}}, Timeout),
  ok.

where(Db) ->
  gproc:where({n, l, {barrel_db, Db}}).

start_link(Backend, DbName, Opts) ->
  Backend:start(DbName, Opts).
