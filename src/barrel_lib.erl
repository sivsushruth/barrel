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

-module(barrel_lib).

-export([store_config/0]).
-export([timestamp/0]).
-export([get_ram_size/0]).
-export([get_disk_size/1]).
-export([get_disk_data/0]).
-export([calculate_size/2]).
-export([to_hex/1]).


store_config() ->
  case gproc:get_env(l, barrel, barrel_store) of
    undefined ->
      DefaultDir = lists:concat(["Barrel.", node()]),
      [{path, filename:absname(DefaultDir)}];
    Config -> Config
  end.


%% @doc unique timestamp
-spec timestamp() -> {integer(), integer()}.
timestamp() ->
  T = erlang:monotonic_time(micro_seconds),
  U = erlang:unique_integer(),
  {T, U}.

%% @doc return the maximum memory allocated to the vm
-spec get_ram_size() -> integer().
get_ram_size() ->
  proplists:get_value(system_total_memory, memsup:get_system_memory_data()).

%% return the disk capacity in bytes corresponding to the path.
-spec get_disk_size(string()) -> integer().
get_disk_size(Path) ->
  DiskData = get_disk_data(),
  get_disk_size(DiskData, split_dir(Path)).

get_disk_size([DiskInfo | Rest], PathParts) ->
  Mount = proplists:get_value(mounted_on, DiskInfo),
  case Mount of
    [] -> get_disk_size(Rest, PathParts);
    _ ->
      MountParts = split_dir(Mount),
      case MountParts -- PathParts of
        [] ->
          Available = proplists:get_value(available, DiskInfo),
          Used = proplists:get_value(used, DiskInfo),
          (Available + Used) * 1024;
        _ ->
          get_disk_size(Rest, PathParts)
      end
  end;
get_disk_size([], _) ->
  -1.

%% @doc Retrieve disk data from os command
%% TODO: add windows support.
%% See http://stackoverflow.com/questions/293780/free-space-in-a-cmd-shell
-spec get_disk_data() -> list().
get_disk_data() ->
  Tokens_1 = string:tokens(os:cmd("df -lkP"), "\n"),
  Tokens_2 = lists:delete(hd(Tokens_1), Tokens_1),
  Tokens_3 = [ string:tokens(Row, " ") || Row <- Tokens_2 ],
  get_disk_data(os:type(), Tokens_3).

get_disk_data({unix,darwin}, RetL) ->
    F = fun(Args) ->
      [Filesystem, Blocks, Used, Available, UsePer, MountedOn] =
        case length(Args) of
          9 ->
            [El_1, El_2, El_3, El_4,_,_,_, El_5, El_6] = Args,
            [El_1, El_2, El_3, El_4, El_5, El_6];
          6 ->
            Args;
          _ ->
            ["","0","0","0","0%",""]
        end,

      [ {filesystem,Filesystem},
        {blocks, list_to_integer(Blocks)},
        {used, list_to_integer(Used)},
        {available, list_to_integer(Available)},
        {use_percentage_str, UsePer},
        {mounted_on, MountedOn}
      ]
        end,
    [ F(Row) || Row <- RetL ];
%% For Ubuntu/Debian, CentOS/RHEL, FreeBSD, Solaris/SmartOS
get_disk_data({unix, Type}, RetL)
  when Type =:= linux; Type =:= freebsd; Type =:= sunos ->
  F = fun([Filesystem, Blocks, Used, Available, UsePer, MountedOn]) ->
    ListToIntF = fun(S) ->
      case catch list_to_integer(S) of
        {'EXIT',_} -> 0;
        V -> V
      end
                 end,
    [{filesystem, Filesystem}, {blocks, ListToIntF(Blocks)},
      {used,  ListToIntF(Used)}, {available ,ListToIntF(Available)},
      {use_percentage_str, UsePer},  {mounted_on, MountedOn}]

      end,
  [ F(Row) || Row <- RetL ];
%% Other OSes not supported
get_disk_data(_,_) ->
  erlang:error(unsupported_os).


%% @doc return the size in byte given an input as integer or string.
%% ex: 10G -> 10737418240
-spec calculate_size(integer() | string(), integer()) -> integer().
calculate_size(I, _Max) when is_integer(I) ->
  I;
calculate_size(S, Max) when is_list(S) ->
  case string:to_integer(S) of
    {error, no_integer} ->
      case S of
        [$.| _] ->
          case string:to_float("0" ++ S) of
            {P, ""} when P =< 1 -> trunc(P * Max);
            _ -> error(bad_size)
          end;
        _ ->
          error(bad_size)
      end;
    {I, ""} -> I;
    {0, [$. | _]} ->
      case string:to_float(S) of
        {P, ""} when P =< 1 -> trunc(P * Max);
        _ -> error(bad_size)
      end;
    {I, Unit} ->
      case string:to_lower(Unit) of
        "eb" -> I*1024*1024*1024*1024*1024*1024;
        "pb" -> I*1024*1024*1024*1024*1024;
        "tb" -> I*1024*1024*1024*1024;
        "gb" -> I*1024*1024*1024;
        "mb" -> I*1024*1024;
        "kb" -> I*1024;
        "e" -> I*1024*1024*1024*1024*1024*1024;
        "p" -> I*1024*1024*1024*1024*1024;
        "t" -> I*1024*1024*1024*1024;
        "g" -> I*1024*1024*1024;
        "m" -> I*1024*1024;
        "k" -> I*1024;
        "eib" -> trunc(I*1.15*1024*1024*1024*1024*1024*1024);
        "pib" -> trunc(I*1.13*1024*1024*1024*1024*1024);
        "tib" -> trunc(I*1.1*1024*1024*1024*1024);
        "gib" -> trunc(I*1.074*1024*1024*1024);
        "mib" -> trunc(I*1.049*1024*1024);
        "kib" -> trunc(I*1.024*1024*1024);
        "%" -> trunc(Max * (I / 100));
        _ -> error(bad_size)
      end
  end.


split_dir(Path) ->
  split_dir(os:type(), Path).

split_dir({unix, _}, Path) ->
  string:tokens(Path, "/");
split_dir({win32, _}, Path) ->
  string:tokens(filename:nativename(Path), "\\");
split_dir(_, _) ->
  erlang:error(unsupported_os).


%% @doc convert a binary integer list to an hexadecimal
-spec to_hex(binary()) -> binary().
to_hex(Str) ->
    to_hex(Str, <<>>).

to_hex(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    HiChar = integer_to_hex_char(Hi),
    LoChar = integer_to_hex_char(Lo),
    to_hex(Tail, <<Acc/binary, HiChar, LoChar>>);
to_hex(<<>>, Acc) ->
    Acc.


integer_to_hex_char(N) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $a - 10 + N;
        true ->
            erlang:error(badarg)
    end.
