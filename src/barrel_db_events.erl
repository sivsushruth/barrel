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

-module(barrel_db_events).
-behaviour(gen_event).
-author("benoitc").

%% API
-export([start_link/0]).

-export([notify/1]).
-export([add_callback/1]).
-export([add_sup_callback/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {callback}).


start_link() ->
  gen_event:start_link({local, ?MODULE}).


notify(Event) ->
  gen_event:notify(?MODULE, Event).

add_callback(Fn) when is_function(Fn) ->
  gen_event:add_handler(?MODULE, {?MODULE, make_ref()}, [Fn]).

add_sup_callback(Fn) when is_function(Fn) ->
  gen_event:add_sup_handler(?MODULE, {?MODULE, make_ref()}, [Fn]).



%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([Fn]) ->
  {ok, #state{callback=Fn}}.

handle_event(Event, State) ->
  (State#state.callback)(Event),
  {ok, State}.

handle_call(_Request, State) ->
  {ok, ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
