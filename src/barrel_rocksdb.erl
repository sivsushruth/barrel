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

-export([open/2,
         close/1,
         destroy/1]).


open(Db, Options) ->
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
      {ok, #{ name => Name,
              handle => Handle }};
    Error ->
      Error
  end.

close(#{ handle := Handle }) ->
  erocksdb:close(Handle).


destroy("") ->
  %% RAM db
  ok;
destroy(Name) ->
  %% we don't use erocksdb:destroy/1 here so we can optimise the deletion
  %% depending on the platform.
  barrel_os_util:rm_rf(Name).

