%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
%% Copyright (c) 2016 Benoît Chesneau
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE


-module(barrel_os_util).


-export([rm_rf/1]).
-export([delete_each/1]).
-export([get_cwd/0]).
-export([sh/2]).
-export([escape_chars/1]).
-export([escape_double_quotes/1]).
-export([escape_double_quotes_weak/1]).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

%%
%% Options = [Option] -- defaults to [use_stdout, abort_on_error]
%% Option = ErrorOption | OutputOption | {cd, string()} | {env, Env}
%% ErrorOption = return_on_error | abort_on_error | {abort_on_error, string()}
%% OutputOption = use_stdout | {use_stdout, bool()}
%% Env = [{string(), Val}]
%% Val = string() | false
%%
sh(Command0, Options0) ->
  lager:debug("sh info:\n\tcwd: ~p\n\tcmd: ~s\n", [get_cwd(), Command0]),
  lager:debug("\topts: ~p\n", [Options0]),

  DefaultOptions = [{use_stdout, false}, debug_and_abort_on_error],
  Options = [expand_sh_flag(V)
             || V <- proplists:compact(Options0 ++ DefaultOptions)],

  ErrorHandler = proplists:get_value(error_handler, Options),
  OutputHandler = proplists:get_value(output_handler, Options),

  Command = lists:flatten(patch_on_windows(Command0, proplists:get_value(env, Options, []))),
  PortSettings = proplists:get_all_values(port_settings, Options) ++
  [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide, eof],
  lager:debug("Port Cmd: ~s\nPort Opts: ~p\n", [Command, PortSettings]),
  Port = open_port({spawn, Command}, PortSettings),

  try
    case sh_loop(Port, OutputHandler, []) of
      {ok, _Output} = Ok ->
        Ok;
      {error, {_Rc, _Output}=Err} ->
        ErrorHandler(Command, Err)
    end
  after
    port_close(Port)
  end.

get_cwd() ->
  {ok, Dir} = file:get_cwd(),
  %% On windows cwd may return capital letter for drive,
  %% for example C:/foobar. But as said in http://www.erlang.org/doc/man/filename.html#join-1
  %% filename:join/1,2 anyway will convert drive-letter to lowercase, so we have to "internalize"
  %% cwd as soon as it possible.
  filename:join([Dir]).



%% @doc Remove files and directories.
%% Target is a single filename, directoryname or wildcard expression.
-spec rm_rf(string()) -> 'ok'.
rm_rf(Target) ->
    case os:type() of
        {unix, _} ->
            EscTarget = escape_chars(Target),
            {ok, []} = sh(?FMT("rm -rf ~s", [EscTarget]),
                                      [{use_stdout, false}, abort_on_error]),
            ok;
        {win32, _} ->
            Filelist = filelib:wildcard(Target),
            Dirs = [F || F <- Filelist, filelib:is_dir(F)],
            Files = Filelist -- Dirs,
            ok = delete_each(Files),
            ok = delete_each_dir_win32(Dirs),
            ok
    end.


delete_each([]) ->
  ok;
delete_each([File | Rest]) ->
  case file:delete(File) of
    ok ->
      delete_each(Rest);
    {error, enoent} ->
      delete_each(Rest);
    {error, Reason} ->
      lager:error("Failed to delete file ~s: ~p\n", [File, Reason]),
      {error, Reason}
  end.

%% escape\ as\ a\ shell\?
escape_chars(Str) when is_atom(Str) ->
  escape_chars(atom_to_list(Str));
escape_chars(Str) ->
  re:replace(Str, "([ ()?`!$&;])", "\\\\&", [global, {return, list}]).

%% "escape inside these"
escape_double_quotes(Str) ->
  re:replace(Str, "([\"\\\\`!$&*;])", "\\\\&", [global, {return, list}]).

%% "escape inside these" but allow *
escape_double_quotes_weak(Str) ->
  re:replace(Str, "([\"\\\\`!$&;])", "\\\\&", [global, {return, list}]).


%% ===================================================================
%% Internal functions
%% ===================================================================

delete_each_dir_win32([]) -> ok;
delete_each_dir_win32([Dir | Rest]) ->
  {ok, []} = sh(?FMT("rd /q /s \"~s\"",
                     [escape_double_quotes(filename:nativename(Dir))]),
                [{use_stdout, false}, return_on_error]),
  delete_each_dir_win32(Rest).


%% We do the shell variable substitution ourselves on Windows and hope that the
%% command doesn't use any other shell magic.
patch_on_windows(Cmd, Env) ->
  case os:type() of
    {win32,nt} ->
      Cmd1 = "cmd /q /c "
      ++ lists:foldl(fun({Key, Value}, Acc) ->
                         expand_env_variable(Acc, Key, Value)
                     end, Cmd, Env),
      %% Remove left-over vars
      re:replace(Cmd1, "\\\$\\w+|\\\${\\w+}", "",
                 [global, {return, list}]);
    _ ->
      Cmd
    end.


%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, RawVarValue) ->
  case string:chr(InStr, $$) of
    0 ->
      %% No variables to expand
      InStr;
    _ ->
      ReOpts = [global, unicode, {return, list}],
      VarValue = re:replace(RawVarValue, "\\\\", "\\\\\\\\", ReOpts),
      %% Use a regex to match/replace:
      %% Given variable "FOO": match $FOO\s | $FOOeol | ${FOO}
      RegEx = io_lib:format("\\\$(~s(\\W|$)|{~s})", [VarName, VarName]),
      re:replace(InStr, RegEx, [VarValue, "\\2"], ReOpts)
  end.

expand_sh_flag(return_on_error) ->
  {error_handler,
   fun(_Command, Err) ->
       {error, Err}
   end};
expand_sh_flag(abort_on_error) ->
  {error_handler,
   fun log_and_abort/2};
expand_sh_flag({abort_on_error, Message}) ->
  {error_handler,
   log_msg_and_abort(Message)};
expand_sh_flag({debug_abort_on_error, Message}) ->
  {error_handler,
   debug_log_msg_and_abort(Message)};
expand_sh_flag(debug_and_abort_on_error) ->
  {error_handler,
   fun debug_and_abort/2};
expand_sh_flag(use_stdout) ->
  {output_handler,
   fun(Line, Acc) ->
       %% Line already has a newline so don't use ?CONSOLE which adds one
       io:format("~s", [Line]),
       [Line | Acc]
   end};
expand_sh_flag({use_stdout, false}) ->
  {output_handler,
   fun(Line, Acc) ->
       [Line | Acc]
   end};
expand_sh_flag({cd, _CdArg} = Cd) ->
  {port_settings, Cd};
expand_sh_flag({env, _EnvArg} = Env) ->
  {port_settings, Env}.


log_msg_and_abort(Message) ->
  fun(_Command, {_Rc, _Output}) ->
      lager:info(Message, []),
      throw(abort)
  end.

debug_log_msg_and_abort(Message) ->
  fun(Command, {Rc, Output}) ->
      lager:debug("sh(~s)~n"
                  "failed with return code ~w and the following output:~n"
                  "~s", [Command, Rc, Output]),
      lager:info(Message, []),
      throw(abort)
  end.

log_and_abort(Command, {Rc, Output}) ->
  lager:info("sh(~s)~n"
             "failed with return code ~w and the following output:~n"
             "~s", [Command, Rc, Output]),
  throw(abort).

debug_and_abort(Command, {Rc, Output}) ->
  lager:debug("sh(~s)~n"
               "failed with return code ~w and the following output:~n"
               "~s", [Command, Rc, Output]),
  throw(abort).



sh_loop(Port, Fun, Acc) ->
  receive
    {Port, {data, {eol, Line}}} ->
      sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
    {Port, {data, {noeol, Line}}} ->
      sh_loop(Port, Fun, Fun(Line, Acc));
    {Port, eof} ->
      Data = lists:flatten(lists:reverse(Acc)),
      receive
        {Port, {exit_status, 0}} ->
          {ok, Data};
        {Port, {exit_status, Rc}} ->
          {error, {Rc, Data}}
      end
  end.
