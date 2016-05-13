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
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2016 18:57
%%%-------------------------------------------------------------------
-module(barrel_rocksdb).
-author("benoitc").

%% DB API
-export([init_backend/0,
         backend_aliases/0,
         create_database/3,
         open_database/3,
         delete_database/1]).

-export([start_link/3]).
-export([init/5]).

%% sys callback functions
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4]).

-define(db_prefix, << 100, 0 >>).
-define(db_key(Db), << 100, 0, (barrel_lib:to_binary(Db))/binary >>).


-define(VERSION, 1).
-define(GLOBAL_DB, rocksdb_global).

init_backend() -> ok.

backend_aliases() -> [rocksdb_ram, rocksdb_disc].

create_database(Alias, Db, Options) when is_list(Options) ->
  open_database(Alias, Db, [{create_if_missing, true},
                            {error_if_exists, true}|Options]);
create_database(_Alias, _Db, Config) ->
  erlang:error({badarg, Config}).


open_database(Alias, Db, Options) when is_atom(Db), is_list(Options) ->
  {ok, _Pid} = barrel_ext_sup:start_proc(Db, ?MODULE, start_link,
                                          [Alias, Db, Options]),
  ok;
open_database(_Alias, _Db, _Options) ->
  erlang:error(badarg).

delete_database(Db) ->
  %% we let the barrel manager delete the files
  req(Db, delete_database).


%% If the db process crashes we want to give the supervisor
%% a decent chance to restart it before failing our calls.
req(Db, Req) ->
  req(Db, Req, 99).

req(Db, Req, 0) ->
  req1(Db, Req);
req(Db, Req, Retries) ->
  case req1(Db, Req) of
    {error, noproc} ->
      timer:sleep(100),
      req(Db, Req, Retries - 1);
    Reply ->
      Reply
  end.

req1(Db, Req) ->
  case whereis(Db) of
    undefined -> {error, db_not_loaded};
    Pid ->
      Ref = make_ref(),
      Pid ! {{self(), Ref}, Req},
      rec(Db, Pid, Ref)
  end.

rec(Db, Pid, Ref) ->
  receive
    {Db, Ref, Reply} ->
      Reply;
    {'EXIT', Pid, _} ->
      {error, {db_not_loaded, Db}}
  end.


start_link(Alias, Db, Options) ->
  Parent = self(),
  NewTab = init_tab(Db),
  proc_lib:start_link(?MODULE, init, [Parent, NewTab, Alias, Db, Options],
                      infinity).


init_tab(Db) ->
  case ets:info(Db, name) of
    undefined ->
      ets:new(Db, [ordered_set, public, named_table,
                   {read_concurrency, true},
                   {write_concurrency, false}]),
      true;
    _ ->
      false
  end.

init(Parent, NewTab, Alias, Db, Options) ->
  register(Db, self()),
  DbRef = case init_db(NewTab, Alias, Db, Options) of
            {ok, Ref} ->
              proc_lib:init_ack(Parent, {ok, self()}),
              Ref;
            Error ->
              exit(Error)
          end,

  loop(#{ sup => Parent,
          db => Db,
          alias => Alias,
          options => Options,
          ref => DbRef }).

loop(#{ sup := Sup} = State) ->
  receive
    {From, delete_database} ->
      do_delete_database(From, State);
    {system, From, Msg} ->
      lager:debug("~p got {system, ~p, ~p}~n", [?MODULE, From, Msg]),
      sys:handle_system_msg(Msg, From, Sup, ?MODULE, [], State);
    Msg ->
      lager:info("~p got an unexpected message: ~p~n", [?MODULE, Msg]),
      loop(State)
  end.



init_db(true, Alias, Db, Options) ->
  IsCreate = proplists:get_value(create_if_missing, Options) =:= true,
  DbDir = barrel_manager:db_dir(Db),
  Res = case {IsCreate, filelib:is_dir(DbDir)} of
          {true, true} -> {error, already_exists};
          {false, false} -> {error, not_found};
          {_, _} ->
            case Alias of
              rocksdb_ram ->
                %% make sure to always create db in ram
                Options1 = lists:ukeysort(1, [{create_if_missing, true} | Options]),
                erocksdb:open("", Options1);
              rocksdb_disc ->
                _ = filelib:ensure_dir(DbDir),
                erocksdb:open(DbDir, Options)
            end
        end,
  case Res of
    {ok, DbRef} ->
      ets:insert(Db, {handle, DbRef}),
      {ok, DbRef};
    Error ->
      Error
  end;
init_db(false, _Alias, Db, _Options) ->
  [DbRef] = ets:lookup(Db, handle),
  {ok, DbRef}.

do_delete_database({Pid, Ref}, #{db := Db, ref := DbRef }) ->
  catch erocksdb:close(DbRef),
  Pid ! {Db, Ref, ok},
  ets:delete(Db),
  exit(shutdown).

do_stop(#{ db := Db, ref := nil}) ->
  ets:delete(Db),
  exit(shutdown);
do_stop(#{ db := Db, ref := DbRef}) ->
  erocksdb:close(DbRef),
  ets:delete(Db),
  exit(shutdown).

%%%%%%%%%%%%%%%%%
%% system upgrade

system_continue(_Parent, _Debug, State) ->
  loop(State).

-spec system_terminate(_, _, _, _) -> no_return().
system_terminate(Reason, _Parent, _Debug, State) ->
  case Reason of
    normal -> do_stop(State);
    shutdown -> do_stop(State);
    {shutdown, _} -> do_stop(State);
    _ -> exit(Reason)
  end.

system_code_change(State, _Module, _OldVsn, _Extra) ->
  {ok, State}.


