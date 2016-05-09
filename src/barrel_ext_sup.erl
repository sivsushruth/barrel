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

%%%-------------------------------------------------------------------
%% @doc barrel supervisor for store backends & plugins
%% @end
%%%-------------------------------------------------------------------

-module(barrel_ext_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_proc/4, start_proc/5]).

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
  DefaultSpec = #{ id => Name,
                   start => {M, F, A},
                   restart => transient,
                   shutdown => ?SHUTDOWN,
                   type => worker,
                   modules => [M] },

  Spec = merge_opts(Opts, DefaultSpec),
  case supervisor:start_child(?MODULE, Spec) of
    {error, already_present} ->
      supervisor:restart_child(?MODULE, Name);
    Other ->
      Other
  end.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, {{one_for_one, 4, 3600}, []}}.


%%====================================================================
%% Internal functions
%%====================================================================

-spec merge_opts([{atom(), any()}], map()) -> map().
merge_opts([{restart, V} | Rest], Spec) ->
  merge_opts(Rest, Spec#{restart => V});
merge_opts([{shutdown, V} | Rest], Spec) ->
  merge_opts(Rest, Spec#{shutdown => V});
merge_opts([{type, V} | Rest], Spec) ->
  merge_opts(Rest, Spec#{type => V});
merge_opts([{modules, V} | Rest], Spec) ->
  merge_opts(Rest, Spec#{modules => V});
merge_opts([_| Rest], Spec) ->
  merge_opts(Rest, Spec).
