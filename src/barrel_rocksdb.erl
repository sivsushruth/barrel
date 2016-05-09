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
         close_database/1,
         delete_database/3]).

-export([enc/1, dec/1]).

-define(db_prefix, << 100, 0 >>).
-define(db_key(DbName), << 100, 0, (barrel_lib:to_binary(DbName))/binary >>).


-define(VERSION, 1).
-define(GLOBAL_DB, rocksdb_global).

-compile({inline, [enc/1, dec/1]}).


init_backend() -> ok.

backend_aliases() -> [rocksdb_ram, rocksdb_disc].

create_database(Type, DbName, Options) when is_list(Options) ->
  open_database(Type, DbName, [{create_if_missing, true},
                               {error_if_exists, true}|Options]);
create_database(_Type, _DbName, Config) ->
  erlang:error({badarg, Config}).


open_database(Type, DbName, Options) when is_list(Options) ->
  IsCreate = proplists:get_value(create_if_missing, Options) =:= true,
  DbDir = barrel_manager:db_dir(DbName),
  case {IsCreate, filelib:is_dir(DbDir)} of
    {true, true} -> {error, already_exists};
    {false, false} -> {error, not_found};
    {_, _} ->
      case Type of
        rocksdb_ram ->
          %% make sure to always create db in ram
          Options1 = lists:ukeysort(1, [{create_if_missing, true} | Options]),
          erocksdb:open("", Options1);
        rocksdb_disc ->
          _ = filelib:ensure_dir(DbDir),
          erocksdb:open(DbDir, Options)
      end
  end;
open_database(_Type, _DbName, Options) ->
  erlang:error({badarg, Options}).


close_database(Db) ->
  erocksdb:close(Db).

delete_database(_alias, _DbName, _Options) ->
  %% we let the barrel manager delete the files
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
%%
%% encode a value. for now we are using the erlang serialisation, the function
%% add a version prefix to make sure we can go back later on it without to care
%% much. Version should be a 32 bits integer.
-spec enc(term()) -> binary().
enc(Term) ->
  << ?VERSION, (term_to_binary(Term))/binary >>.

%% decode a value.
-spec dec(binary()) -> term().
dec(<< ?VERSION, Bin/binary >>) ->
  binary_to_term(Bin).
