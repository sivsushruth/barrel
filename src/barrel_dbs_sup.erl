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

-module(barrel_dbs_sup).
-behaviour(supervisor).
-author("benoitc").

%% API
-export([start_link/0]).
-export([start_db/3]).
-export([stop_db/1]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags =  {simple_one_for_one, 0, 10},
  Db = {barrel_db,
        {barrel_controller, start_link, []},
        transient, 5000, worker, [barrel_db]},

  {ok, {SupFlags, [Db]}}.


start_db(Backend, DbName, Options) ->
  supervisor:start_child(?MODULE, [Backend, DbName, Options]).

stop_db(DbName) ->
  case gproc:where({n, l, {barrel_db, DbName}}) of
    Pid when is_pid(Pid) ->
      supervisor:terminate_child(?MODULE, Pid);
    _ ->
      {error, unknown_db}
  end.
