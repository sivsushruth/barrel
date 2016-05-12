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
%% @doc barrel supervisor for store backends & plugins
%% @end
%%%-------------------------------------------------------------------

-module(barrel_ext_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_proc/4, start_proc/5]).
-export([stop_proc/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN, 120000).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_proc(Name, M, F, A) ->
    start_proc(Name, M, F, A, []).

start_proc(Name, M, F, A, Opts) ->
    [Restart, Shutdown, Type, Modules] =
	[proplists:get_value(K, Opts, Default)
	 || {K, Default} <- [{restart, transient},
			     {shutdown, ?SHUTDOWN},
			     {type, worker},
			     {modules, [M]}]],
    case supervisor:start_child(
	   ?MODULE, {Name, {M,F,A}, Restart, Shutdown, Type, Modules}) of
	{error, already_present} ->
	    supervisor:restart_child(?MODULE, Name);
	Other ->
	    Other
    end.


stop_proc(Name) ->
  supervisor:terminate_child(?MODULE, Name).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, {{one_for_one, 4, 3600}, []}}.
