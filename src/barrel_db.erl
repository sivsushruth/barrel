%% Copyright 2016 benoitc
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

-module(barrel_db).
-author("benoitc").

%% API
-export([await/1, await/2]).
-export([start_link/3]).
-export([init/4]).

-export([req/2, get_res/1]).

%% sys callback functions
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4]).


await(Db) ->
  await(Db, timer:minutes(1)).

await(Db, Timeout) ->
  gproc:await({n, l, {?MODULE, Db}}, Timeout),
  ok.

start_link(Backend, Db, Options) ->
  Parent = self(),
  proc_lib:start_link(?MODULE, init, [Parent, Backend, Db, Options],
                      infinity).


%%  get the db resource, like req/2 we retry it to give a chance to the supervisor
%% to start the process
get_res(Db) ->
	get_res(Db, 99).

get_res(Db, 0) ->
	try
		gproc:get_value({n, l, {?MODULE, Db}})
	catch
		exit:_ -> exit({db_not_loaded, Db})
	end;
get_res(Db, Retries) ->
	try
		gproc:get_value({n, l, {?MODULE, Db}})
	catch
		exit:_ ->
			timer:sleep(100),
			get_res(Db, Retries -1)
	end.

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
  case gproc:where({n, l, {?MODULE, Db}}) of
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



init(Parent, Backend, Db, Options) ->
  process_flag(trap_exit, true),
  DbRes = case Backend:open(Db, Options) of
            {ok, Res} ->
              gproc:reg({n, l, {?MODULE, Db}}, {Backend, Res}),
              proc_lib:init_ack(Parent, {ok, self()});
            Error ->
              exit(Error)
          end,

  db_loop(#{ sup => Parent,
             backend => Backend,
             db => Db,
             options => Options,
             res => DbRes }).


db_loop(#{sup := Sup} = State) ->
  receive
    {system, From, Msg} ->
      lager:debug("~p got {system, ~p, ~p}~n", [?MODULE, From, Msg]),
      sys:handle_system_msg(Msg, From, Sup, ?MODULE, [], State);

    {'EXIT', Sup, _Reason} ->
      do_stop(State);
    Msg ->
      lager:info("~p got an unexpected message: ~p~n", [?MODULE, Msg]),
      db_loop(State)
  end.


do_stop(#{ res := nil}) ->
  exit(shutdown);
do_stop(#{ backend := Backend, res := DbRes}) ->
  catch Backend:close(DbRes),
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
      #{ backend := Backend, res := DbRes } = State,
      catch Backend:close(DbRes),
      exit(Reason)
  end.

system_code_change(State, _Module, _OldVsn, _Extra) ->
  {ok, State}.
