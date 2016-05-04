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
%% @doc main db supervisor, a database is started using the
%% barrel_dbs_sup:start_child/2 and stopped using
%% barrel_db_sup:stop_child/1

-module(barrel_dbs_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/2]).

%% Supervisor callback
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, { {simple_one_for_one, 0, 10},
         [{id, {barrel_db_sup, start_link, []},
           permanent, infinity, supervisor, [barrel_db_sup]}] }}.

start_child(DbName, Options) ->
  supervisor:start_child(?MODULE, [DbName, Options]).
