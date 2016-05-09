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
-export([close_database/1]).
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

-record(state, {supervisor :: pid(),
                pending :: list()}).
-record(pending, {db, ref, from, reqtype, clients}).

-define(DBS, barrel_dbs).
-define(CLIENTS, barrel_dbs_clients).
-define(OWNERS, barrel_dbs_owners).

-define(DEFAULT_BACKENDS, #{ rocksdb_ram => barrel_rocksdb,
                             rocksdb_disc => barrel_rocksdb}).
-define(DEFAULT_ALIAS, rocksdb_disc).


-spec all_databases() -> [barrel:dbname()].
all_databases() ->
  Fun = fun(DbName, Acc) -> [DbName | Acc] end,
  lists:reverse(all_databases(Fun, [])).


-spec all_databases(barrel:all_databases_fun(), any()) -> any().
all_databases(Fun, AccIn) ->
  {ok, Dirs} = file:list_dir(barrel_store_config:data_dir()),
  lists:foldl(fun
                (".", Acc) -> Acc;
                ("..", Acc) -> Acc;
                (Dir, Acc) ->
                  case filelib:is_file(options_file(Dir)) of
                    true -> Fun(Dir, Acc);
                    false -> Acc
                  end
              end, AccIn, Dirs).

-spec create_database(atom(), barrel:dbname(), list()) -> {ok, barrel:db()} | {error, term()}.
create_database(Alias, DbName, Options) ->
  gen_server:call(?MODULE, {create_database, Alias, DbName, Options}).

-spec open_database(barrel:dbname()) -> {ok, barrel:db()} | {error, term()}.
open_database(DbName) ->
  gen_server:call(?MODULE, {open_database, DbName}).

-spec close_database(barrel:db()) -> ok | {error, term()}.
close_database(#{ name := DbName} = Db) ->
  gen_server:call(?MODULE, {close_database, DbName, Db}).

-spec delete_database(barrel:db()) -> ok | {error, term()}.
delete_database(DbName) ->
    gen_server:call(?MODULE, {delete_database, DbName}).


-spec options_file(barrel:dbname()) -> list().
options_file(DbName) ->
  filename:join([db_dir(DbName), "BARREL_OPTIONS"]).

-spec db_dir(string()) -> string().
db_dir(DbName) when is_list(DbName) ->
    filename:join([barrel_store_config:data_dir(), DbName]);
db_dir(DbName) ->
     erlang:error({baddb, DbName}).

-spec store_options(atom(), barrel:dbname(), list()) -> ok | {error, term()}.
store_options(Alias, DbName, Options) ->
  OptionsFile = options_file(DbName),
  filelib:ensure_dir(OptionsFile),
  {ok, Fd} = file:open(OptionsFile, [write]),
  lists:foreach(fun({K, V}) ->
                    io:fwrite(Fd, "{~p, ~p}.~n", [K, V])
                end, [{backend_alias, Alias} | Options]),
  file:sync(Fd),
  file:close(Fd).

-spec load_latest_options(barrel:dbname()) -> {ok, atom(), list()} | {error, term()}.
load_latest_options(DbName) ->
  OptionsFile = options_file(DbName),
  case filelib:is_file(OptionsFile) of
    true ->
      case file:consult(OptionsFile) of
        {ok, Options} ->
          case proplists:get_value(backend_alias, Options) of
            undefined ->
              {error, {invalid_options, DbName}};
            Alias ->
              {ok, Alias, lists:keydelete(backend_alias, 1, Options)}
          end;
        Error ->
          Error
      end;
    false ->
      {error, not_found}
  end.

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
  InitMonitors = init_tabs(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [InitMonitors], []).


init_tabs() ->
  case ets:info(?DBS, name) of
    undefined -> ets:new(?DBS, [ordered_set, named_table, public]);
    _ -> ok
  end,

  case ets:info(?OWNERS, name) of
    undefined -> ets:new(?OWNERS, [bag, named_table, public]);
    _ -> ok
  end,

  case ets:info(?CLIENTS, name) of
    undefined ->
      ets:new(?CLIENTS, [set, named_table, public]),
      false;
    _ ->
      true
  end.


init([InitMonitors]) ->
  _ = init_monitors(InitMonitors),
  {ok, #state{pending=[]}}.


handle_call({create_database, _, _, _}=Req, From, State) ->
  handle_request([{From, Req}], State);
handle_call({open_database, _}=Req, From, State) ->
  handle_request([{From, Req}], State);
handle_call({close_database, _, _}=Req, From, State) ->
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


handle_request([{From, Req} | Rest], State) ->
  Res = case Req of
          {create_database, _Alias, DbName, _Options} ->
            handle_create(State, Req, From, DbName);
          {open_database, DbName} ->
            handle_open(State, Req, From, DbName);
          {close_database, DbName, _Db} ->
            handle_close(State, Req, From, DbName);
          {delete_database, DbName} ->
            handle_delete(State, Req, From, DbName)
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
  #pending{db = DbName, from=From, clients=Clients} = Pending,
  gen_server:reply(From, Result),
  NP = lists:keydelete(DbName, #pending.db, State#state.pending),
  handle_request(Clients, State#state{pending = NP}).

check_pending(DbName, From, #state{pending=P}=State, Req) ->
  case lists:keysearch(DbName, #pending.db, P) of
    {value, #pending{db = DbName, clients=Clients}=P} ->
      NP = lists:keyreplace(DbName, #pending.db, State#state.pending,
                            P#pending{clients=Clients++[{Req,From}]}),
      {pending, State#state{pending=NP}};
    false ->
      false
  end.

pending_call(DbName, {FromPid, _}=From, Req, State) ->
  Ref = make_ref(),
  Server = self(),
  F = fun() ->
          Res = case Req of
                  {create_database, Alias, DbName, Options} ->
                    do_create_database(Alias, DbName, Options, FromPid);
                  {open_database, DbName} ->
                    do_open_database(DbName, FromPid);
                  {close_database, DbName, Db} ->
                    do_close_database(Db, FromPid);
                  {delete_database, DbName} ->
                    do_delete_database(DbName, FromPid)
                end,

          Server ! {pending_reply, {Ref, Res}}
      end,
  _ = spawn(F),
  PD = #pending{db=DbName, ref=Ref, from=From, clients=[]},
  P = [PD | State#state.pending],
  {pending, State#state{pending=P}}.


handle_create(State, Req, From, DbName) ->
  case check_pending(DbName, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      case ets:lookup(?DBS, DbName) of
        [] ->
          pending_call(DbName, From, Req, State);
        [_] ->
          {{error, already_exists}, State}
      end
  end.

handle_open(State, Req, {FromPid, _} = From, DbName) ->
  case check_pending(DbName, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      case ets:lookup(?DBS, DbName) of
        [] ->
          pending_call(DbName, From, Req, State);
        [{DbName, Db, _}] ->
          ets:update_counter(?DBS, DbName, {3, 1}),
          ets:insert(?OWNERS, {DbName, FromPid}),
          monitor_client(FromPid),
          {{ok, Db}, State}
      end
  end.

handle_close(State, {close_database, _, Db}=Req, {FromPid, _} = From, DbName) ->
  case check_pending(DbName, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      Reply = do_close_database(Db, FromPid),
      {Reply, State}
  end.

handle_delete(State, Req, {FromPid, _} = From, DbName) ->
  case check_pending(DbName, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      Reply = do_delete_database(DbName, FromPid),
      {Reply, State}
  end.



do_create_database(Alias, DbName, Options, FromPid) ->
  case ets:lookup(?DBS, DbName) of
    [] ->
      internal_create_database(Alias, DbName, Options, FromPid);
    [_] ->
      {error, already_exists}
  end.

internal_create_database(Alias, DbName, Options, FromPid) ->
  case backend(Alias) of
    {ok, Mod} ->
      case Mod:create_database(Alias, DbName, Options) of
        {ok, DbRef} ->
          store_options(Alias, DbName, Options),
          Db = #{ backend => Mod,
                  alias => Alias,
                  db => DbRef,
                  name => DbName },

          ets:insert(?DBS, {DbName, Db, 1}),
          ets:insert(?OWNERS, {DbName, FromPid}),
          monitor_client(FromPid),

          {ok, Db};
        Error ->
          Error
      end;
    error ->
      {error, bad_alias}
  end.

do_open_database(DbName, FromPid) ->
  Res = case ets:lookup(?DBS, DbName) of
         [] ->
            internal_open(DbName);
          [{DbName, Db, _}] ->
            ets:update_counter(?DBS, DbName, {3, 1}),
            {ok, Db}
        end,
  case Res of
    {ok, _Db} ->
      ets:insert(?OWNERS, {DbName, FromPid}),
      monitor_client(FromPid),
      Res;
    Error ->
      Error
  end.

internal_open(DbName) ->
  case load_latest_options(DbName) of
    {ok, Alias, Options} ->
      case backend(Alias) of
        {ok, Mod} ->
          case Mod:open_database(Alias, DbName, Options) of
            {ok, DbRef} ->
              Db = #{ backend => Mod,
                      alias => Alias,
                      db => DbRef,
                      name => DbName },
              ets:insert(?DBS, {DbName, Db, 1}),
              {ok, Db};
            Error ->
              Error
          end;
        error ->
          {error, bad_alias}
      end;
    Error ->
      Error
  end.


do_close_database(Db, FromPid) ->
  #{ backend := Mod, db := DbRef, name := DbName } = Db,
  ets:delete(?OWNERS, {DbName, FromPid}),
  case ets:lookup(?DBS, DbName) of
    [] -> ok;
    [_] ->
      case ets:update_counter(?DBS, DbName, {3, -1}) of
        0 ->
          ets:delete(?DBS, DbName),
          Mod:close_database(DbRef);
        _ ->
          ok
      end
  end.


do_delete_database(DbName, FromPid) ->
	case ets:lookup(?DBS, DbName) of
		[] -> ok;
		[{_, Db, _}] ->
			_ = do_close_database(Db, FromPid)
	end,

	case load_latest_options(DbName) of
		{ok, Alias, Options} ->
			case backend(Alias) of
				{ok, Mod} ->
					Mod:delete_database(Alias, DbName, Options);
				_ ->
					ok
			end,
			ok = barrel_os_util:rm_rf(db_dir(DbName)),
			ok;
		{error, not_found} ->
			ok
	end.

monitor_client(Pid) ->
  case ets:insert_new(?CLIENTS, {Pid, m}) of
    false ->
      erlang:monitor(process, Pid);
    true ->
      ok
  end.

init_monitors(false) ->
  ok;
init_monitors(true) ->
  [erlang:monitor(process, Pid) || [Pid] <- ets:match(?CLIENTS, {'$1', m})].
