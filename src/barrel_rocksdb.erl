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

-export([start/2,
         stop/1,
         drop/1]).

-export([init/3]).

%% sys callback functions
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4]).

-define(VERSION, 1).

start(Db, Options) ->
  Parent = self(),
  proc_lib:start_link(?MODULE, init, [Parent, Db, Options], infinity).


stop(Db) ->
  req(Db, close).

drop(Db) ->
  req(Db, destroy).


%% If the db process crashes we want to give the supervisor
%% a decent chance to restart it before failing our calls.
req(Db, Req) ->
  req(Db, Req, 99).

req(Db, Req, 0) ->
  req1(Db, Req);
req(Db, Req, Retries) ->
  try
    req1(Db, Req)
  catch
    exit:{db_not_loaded, _} ->
      timer:sleep(100),
      req(Db, Req, Retries - 1)
  end.

req1(Db, Req) ->
  case barrel_controller:where(Db) of
    undefined -> exit({db_not_loaded, Db});
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
      exit({db_not_loaded, Db})
  end.




init(Parent, Db, Options) ->
  process_flag(trap_exit, true),
  Handle = case do_open(Db, Options) of
            {ok, H} ->
              barrel_controller:register_db(Db, ?MODULE),
              proc_lib:init_ack(Parent, {ok, self()}),
              H;
            Error ->
              exit(Error)
          end,

  State =#{sup => Parent,
           db => Db,
           handle => Handle,
           options => Options},

  db_loop(State).


db_loop(#{sup := Sup} = State) ->
  receive
    {From, close} ->
      handle_close(From, State);
    {From, destroy} ->
      handle_destroy(From, State);
    {system, From, Msg} ->
      lager:debug("~p got {system, ~p, ~p}~n", [?MODULE, From, Msg]),
      sys:handle_system_msg(Msg, From, Sup, ?MODULE, [], State);

    {'EXIT', Sup, _Reason} ->
      do_stop(State);
    Msg ->
      lager:info("~p got an unexpected message: ~p~n", [?MODULE, Msg]),
      db_loop(State)
  end.

do_open(Db, Options) ->
  Name = case proplists:get_value(file, Options) of
           undefined ->
             barrel_lib:db_file(Db);
           "" ->
             %% DB in ram
             "";
           File ->
             barrel_lib:db_file(File)
         end,


  DbOpts = lists:ukeysort(1, [{create_if_missing, true} |
                              proplists:get_value(db_opts, Options, [])]),
  case erocksdb:open(Name, DbOpts) of
    {ok, Handle} ->

      {ok, Handle};
    Error ->
      Error
  end.

handle_close({Pid, Ref}, #{ db := Db , handle := Handle}) ->
  erocksdb:close(Handle),
  Pid ! {Db, Ref, ok},
  ok.

handle_destroy({Pid, Ref}, #{ db := Db, handle := Handle }) ->
  erocksdb:close(Handle),
  case erocksdb:destroy(Db, []) of
    ok ->
      Pid ! {Db, Ref, ok};
    Error ->
      Pid ! {Db, Ref, Error}
  end,
  ok.

do_stop(#{ handle := Handle}) ->
  catch erocksdb:close(Handle),
  exit(shutdown).

%%%%%%%%%%%%%%%%%
%% system upgrade

system_continue(_Parent, _Debug, State) ->
  db_loop(State).

-spec system_terminate(_, _, _, _) -> no_return().
system_terminate(Reason, _Parent, _Debug, State) ->
  lager:info("~p terminate: ~p~n", [?MODULE, Reason]),
  case Reason of
    normal -> do_stop(State);
    shutdown -> do_stop(State);
    {shutdown, _} -> do_stop(State);
    _ ->
      #{ handle := Handle } = State,
      catch erockdb:close(Handle),
      exit(Reason)
  end.

system_code_change(State, _Module, _OldVsn, _Extra) ->
  {ok, State}.
