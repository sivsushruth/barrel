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

%%%-------------------------------------------------------------------
%% @doc barrel top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(barrel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = {one_for_all, 0, 3600},

  _ = init_tabs(),

 Manager = {barrel_manager,
            {barrel_manager, start_link, []},
            permanent, 5000, worker, [barrel_manager]},

  %% safe supervisor, like kernel_safe_sup but for barrel, allows to register
  %% external applications to it like stores if needed.
 ExtSup = {barrel_ext_sup,
           {barrel_ext_sup, start_link, []},
           permanent, infinity, supervisor, [barrel_ext_sup]},


  {ok, { SupFlags, [Manager, ExtSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
%%
init_tabs() ->
  _ = ets:new(barrel_gvar, [set, named_table, public]),
  ok.
