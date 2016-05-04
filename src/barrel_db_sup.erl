%% Copyright 2016 Enki Multimedia
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
%%
%%
%% @doc database supervisor that manage the process for a database.

-module(barrel_db_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).
-export([stop_child/1]).

%% Supervisor callback
-export([init/1]).


start_link(DbName, Options) ->
  supervisor:start_link(?MODULE, {DbName, Options}).

init({DbName, Options}) ->
  gproc:reg({n, l, {?MODULE, DbName}}),

  {ok, {{rest_for_one, 5, 10},
        [{db, {barrel_db, start_link, [DbName, Options]},
          permanent, 5000, worker, [barrel_db]}]}}.

stop_child(DbName) ->
  case gproc:where({n,l,{?MODULE, DbName}}) of
    Pid when is_pid(Pid) ->
      supervisor:terminate_child(barrel_dbs_sup, Pid);
    _ ->
      {error, unknown_db}
  end.
