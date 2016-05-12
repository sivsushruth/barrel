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
%%% @author Benoit Chesneau
%%% @copyright (C) 2016, Enki Multimedia
%%% @doc database manager
%%%
%%% @end
%%% Created : 21. Apr 2016 18:57
%%%-------------------------------------------------------------------


-module(barrel_manager).
-behaviour(gen_server).

%% API
-export([all_databases/0, all_databases/2]).
-export([create_database/3]).
-export([open_database/1]).
-export([delete_database/1]).


-export([options_file/1]).
-export([store_options/3]).
-export([load_latest_options/1]).
-export([backend/1]).
-export([backends/0]).
-export([db_dir/1]).

-export([start_link/0]).

%% Server callbacls
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-include("barrel.hrl").

-record(state, {pending :: list()}).
-record(pending, {db, ref, from, reqtype, clients}).


-define(DEFAULT_BACKENDS, #{ rocksdb_ram => barrel_rocksdb,
                             rocksdb_disc => barrel_rocksdb}).
-define(DEFAULT_ALIAS, rocksdb_disc).


-spec all_databases() -> [barrel:dbname()].
all_databases() ->
  Fun = fun(Db, Acc) -> [Db | Acc] end,
  lists:sort(all_databases(Fun, [])).


-spec all_databases(barrel:all_databases_fun(), any()) -> any().
all_databases(Fun, AccIn) ->
  {ok, Dirs} = file:list_dir(barrel_store_config:data_dir()),
  lists:foldl(fun
                (".", Acc) -> Acc;
                ("..", Acc) -> Acc;
                (Dir0, Acc) ->
                  Dir = list_to_atom(Dir0),
                  case filelib:is_file(options_file(Dir)) of
                    true -> Fun(Dir, Acc);
                    false -> Acc
                  end
              end, AccIn, Dirs).

-spec create_database(atom(), barrel:db(), list()) -> ok | {error, term()}.
create_database(Alias, Db, Options) ->
  gen_server:call(?MODULE, {create_database, Alias, Db, Options}).

-spec open_database(barrel:db()) -> ok | {error, term()}.
open_database(Db) ->
  gen_server:call(?MODULE, {open_database, Db}).

-spec delete_database(barrel:db()) -> ok | {error, term()}.
delete_database(Db) ->
    gen_server:call(?MODULE, {delete_database, Db}).

backend(Alias) ->
  Backends = barrel_store_config:backends(),
  maps:find(Alias, Backends).

backends() ->
  barrel_store_config:backends().

init_backends() ->
  UserBackends = application:get_env(barrel, backends, []),
  lists:foldl(fun(Mod, B) ->
                  case Mod:init_backend() of
                    ok ->
                      Aliases = Mod:backends_aliases(),
                      lists:foldl(fun(Alias, B1) ->
                                      B1#{ Alias => Mod}
                                  end, B, Aliases);
                    Error ->
                      lager:error("Error: init backend ~p: ~p~n", [Mod, Error]),
                      B
                  end
              end, ?DEFAULT_BACKENDS, UserBackends).


load_store_config() ->
  DataDir = barrel_lib:data_dir(),
  Backends = init_backends(),
  DefaultBackend = application:get_env(barrel, default_backend,
                                       ?DEFAULT_ALIAS),
  StoreConfig = [{data_dir, DataDir},
                 {backends, Backends},
                 {default_backend, DefaultBackend}],
  barrel_lib:load_config(barrel_store_config, StoreConfig).


start_link() ->
  _ = load_store_config(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
  {ok, #state{pending=[]}}.


handle_call({create_database, _, _, _}=Req, From, State) ->
  handle_request([{From, Req}], State);
handle_call({open_database, _}=Req, From, State) ->
  handle_request([{From, Req}], State);
handle_call({delete_database, _}=Req, From, State) ->
  handle_request([{From, Req}], State);

handle_call(_Msg, _From, State) ->
  {reply, bad_call, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({pending_reply, {Ref, Result}}, State) ->
  handle_pending_reply({Ref, Result}, State);
handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, Store, _Extra) ->
  {ok, Store}.

terminate(_Reason, _State) ->
  ok.


-spec options_file(barrel:dbname()) -> list().
options_file(Db) ->
  filename:join([db_dir(Db), "BARREL_OPTIONS"]).

-spec db_dir(string()) -> string().
db_dir(Db) when is_atom(Db) ->
    filename:join([barrel_store_config:data_dir(), atom_to_list(Db)]);
db_dir(Db) ->
     erlang:error({baddb, Db}).

-spec store_options(atom(), barrel:db(), list()) -> ok | {error, term()}.
store_options(Alias, Db, Options) ->
  OptionsFile = options_file(Db),
  filelib:ensure_dir(OptionsFile),
  {ok, Fd} = file:open(OptionsFile, [write]),
  lists:foreach(fun({K, V}) ->
                    io:fwrite(Fd, "{~p, ~p}.~n", [K, V])
                end, [{backend_alias, Alias} | Options]),
  file:sync(Fd),
  file:close(Fd).

-spec load_latest_options(barrel:db()) -> {ok, atom(), list()} | {error, term()}.
load_latest_options(Db) ->
  OptionsFile = options_file(Db),
  case filelib:is_file(OptionsFile) of
    true ->
      case file:consult(OptionsFile) of
        {ok, Options} ->
          case proplists:get_value(backend_alias, Options) of
            undefined ->
              {error, {invalid_options, Db}};
            Alias ->
              {ok, Alias, lists:keydelete(backend_alias, 1, Options)}
          end;
        Error ->
          Error
      end;
    false ->
      {error, not_found}
  end.

handle_request([{From, Req} | Rest], State) ->
  Res = case Req of
          {create_database, _Alias, Db, _Options} ->
            handle_create(State, Req, From, Db);
          {open_database, Db} ->
            handle_open(State, Req, From, Db);
          {delete_database, Db} ->
            handle_delete(State, Req, From, Db)
        end,
  State2 = case Res of
             {pending, State1} ->
               State1;
             {Reply, State1} ->
               gen_server:reply(From, Reply),
               State1
           end,
  handle_request(Rest, State2);
handle_request([], State) ->
  {noreply, State}.

handle_pending_reply({Ref, Result}, State) ->
  {value, Pending} = lists:keysearch(Ref, #pending.ref, State#state.pending),
  #pending{db = Db, from=From, clients=Clients} = Pending,
  gen_server:reply(From, Result),
  NP = lists:keydelete(Db, #pending.db, State#state.pending),
  handle_request(Clients, State#state{pending = NP}).

check_pending(Db, From, #state{pending=P}=State, Req) ->
  case lists:keysearch(Db, #pending.db, P) of
    {value, #pending{db = Db, clients=Clients}=P} ->
      NP = lists:keyreplace(Db, #pending.db, State#state.pending,
                            P#pending{clients=Clients++[{Req,From}]}),
      {pending, State#state{pending=NP}};
    false ->
      false
  end.

pending_call(Db, From, Req, State) ->
  Ref = make_ref(),
  Server = self(),
  F = fun() ->
          Res = case Req of
                  {create_database, Alias, Db, Options} ->
                    do_create_database(Alias, Db, Options);
                  {open_database, Db} ->
                    do_open_database(Db);
                  {delete_database, Db} ->
                    do_delete_database(Db)
                end,

          Server ! {pending_reply, {Ref, Res}}
      end,
  _ = spawn(F),
  PD = #pending{db=Db, ref=Ref, from=From, clients=[]},
  P = [PD | State#state.pending],
  {pending, State#state{pending=P}}.



handle_create(State, Req, From, Db) ->
	case check_pending(Db, From, State, Req) of
		{pending, NewState} -> {pending, NewState};
		false ->
      case barrel_lib:get_db(Db) of
        undefined ->
					pending_call(Db, From, Req, State);
				{_, _} ->
					{{error, already_exists}, State}
			end
	end.

handle_open(State, Req, From, Db) ->
  case check_pending(Db, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      case barrel_lib:get_db(Db) of
        undefined ->
          pending_call(Db, From, Req, State);
        {Mod, Alias} ->
          {{ok, {Mod, Alias}}, State}
      end
  end.

handle_delete(State, Req, From, Db) ->
  case check_pending(Db, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      Reply = do_delete_database(Db),
      {Reply, State}
  end.


is_options(Db) ->
  OptionsFile = options_file(Db),
  filelib:is_file(OptionsFile).


do_create_database(Alias, Db, Options) ->
  case backend(Alias) of
    {ok, Mod} ->
      case is_options(Db) of
        true -> {error, already_exists};
        false ->
          case Mod:create_database(Alias, Db, Options) of
            ok ->
              store_options(Alias, Db, Options),
              barrel_lib:set_db(Db, Mod, Alias),
              {ok, {Mod, Alias}};
            Error ->
              Error
          end
      end;
    error ->
      {error, bad_alias}
  end.

do_open_database(Db) ->
  case load_latest_options(Db) of
    {ok, Alias, Options} ->
      case backend(Alias) of
        {ok, Mod} ->
          case Mod:open_database(Alias, Db, Options) of
            ok ->
              barrel_lib:set_db(Db, Mod, Alias),
              {ok, {Mod, Alias}};
            Error ->
              Error
          end;
        error ->
          {error, bad_alias}
      end;
    Error ->
      Error
  end.

do_delete_database(Db) ->
  case barrel_lib:get_db(Db) of
    {Mod, _Alias} ->
      barrel_lib:delete_db(Db),
      Res = (catch Mod:delete_database(Db)),
      case Res of
        ok ->
          barrel_os_util:rm_rf(db_dir(Db)),
          ok;
        {'EXIT', ExitReason} ->
          {error, ExitReason}
      end;
    undefined ->
      ok
  end.
