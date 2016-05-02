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

-module(barrel_store).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).


-define(DEFAULT_MEMBACKEND, barrel_in_mem).
-define(DEFAULT_DISKBACKEND, barrel_rocksdb).
-define(SERVER, ?MODULE).

-type store_context() :: {atom(), pid() | term()}.

-record(store, {ctx :: store_context()}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  Config = barrel_lib:store_config(),
  Backend = storage_backend(Config),

  case Backend:init(Config) of
    {ok, Ctx} ->
      Store = #store{ctx = Ctx},
      register_store(Store),
      {ok, Store};
    Error ->
      {stop, Error}
  end.

handle_call(_Msg, _From, Store) ->
  {reply, bad_call, Store}.

handle_cast(_Msg, Store) ->
  {noreply, Store}.

handle_info(_Info, Store) ->
  {noreply, Store}.

code_change(_OldVsn, Store, _Extra) ->
  {ok, Store}.

terminate(_Reason, #store{ctx=Ctx}=Store) ->
  _ = ?MODULE:close(Ctx),
  unregister_store(Store),
  ok.


register_store(Store) ->
  gproc:reg({n, l, barrel_store}, Store#store.ctx),
  ok.

unregister_store(_Store) ->
  gproc:unreg({n, l, barrel_store}),
  ok.

storage_backend(Config) ->
  Backends = application:get_env(barrel, backends, []),
  case proplists:get_value(type, Config) of
    undefined ->
      case proplists:get_value(path, Config) of
        undefined -> erlang:error(bad_storage_backend);
        _Path -> storage_backend(disk, Backends)
      end;
    Type ->
      storage_backend(Type, Backends)
  end.

storage_backend(disk, Backends) ->
  proplists:get_value(disk, Backends, ?DEFAULT_DISKBACKEND);
storage_backend(mem, Backends) ->
  proplists:get_value(mem, Backends, ?DEFAULT_MEMBACKEND);
storage_backend(Type, Backends) ->
  case proplists:get_value(Type, Backends) of
    undefined -> erlang:error({unsupported_storage, Type});
    Mod -> Mod
  end.
