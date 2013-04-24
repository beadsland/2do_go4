%% CDDL HEADER START    -*-Erlang-*-
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Rudimentary 2do_go4 interpreter.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.0.7

-define(module, dogo).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.0.7").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

-import(gen_command).
-import(pose_open).

-import(re).
-import(file).
-import(filename).

-import(os).
-import(string).
-import(io_lib).

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2]).

% private exports
-export([loop/2]).

%%
%% API Functions
%%

-spec start() -> no_return().
%% @equiv start([])
start() -> start([]).

-spec start(Param :: [atom()]) -> no_return().
%% @doc Start as a blocking function.
start(Param) -> gen_command:start(Param, ?MODULE).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% doc Start as a `pose' command.
run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).

%%
%% Callback Functions
%%

%% @private Callback entry point for gen_command behaviour.
do_run(IO, ARG) ->
  Format = "Starting Dogo ~s list interpreter ~p~n",
  ?STDOUT(Format, [?VERSION(?MODULE), self()]),
  case ARG#arg.v of
    []			-> ?CAPTLN,
                   ?MODULE:loop(IO, {ARG#arg.cmd, [stdin], []});
    [File | _V]	-> case file:get_cwd() of
                     {ok, Dir}			->
                       transclude(IO, {ARG#arg.cmd, [Dir ++ "/."], []}, File), 
                       exit(ok);
                     {error, Reason}	->
                       exit({ARG#arg.cmd, {cwd, Reason}})
                   end
  end.

transclude(IO, {Cmd, [Last | Trans], Status}, File) ->
  Dir = filename:dirname(Last),
  RealFile = realname(File, Dir),
  ReadPid = pose_open:read(RealFile),
  NewIO = ?IO(ReadPid, IO#std.out, IO#std.err),
  ?CAPTLN(ReadPid),
  ?MODULE:loop(NewIO, {Cmd, [RealFile, Last] ++ Trans, [IO#std.in | Status]}).

realname(File, Dir) ->
  AbsFile = filename:absname(File, Dir),
  AbsDir = filename:dirname(AbsFile),
  {PathSep, CmdSep, Pwd} = os_syntax(),
  [First | Rest] = string:tokens(AbsDir, "/\\"),
  case First of
    []  -> [Second | [Third | [Fourth | Remain]]] = Rest,
           case Second of
             []   -> Format = "pushd \\\\~s\\~s & cd \\",                % UNC
                     Pushd = io_lib:format(Format, [Third, Fourth]),
                     Path = Remain;
             _    -> Pushd = "cd \\",                                    % UNIX
                     Path = Rest            
           end;
    _   -> Pushd = io_lib:format("pushd ~s & cd /", [First]),            % Win32
           Path = Rest
  end,
  case os:type() of
    {win32, _}  -> Shell = trim(os:cmd("echo %ComSpec%")), COpt = "/C";
    {unix, _}   -> Shell = "/bin/sh", COpt = "-c"
  end,

  CdSeq = [io_lib:format("cd ~s", [X]) || X <- Path],  
  CdCmd = [io_lib:format("~s ~s ", [X, CmdSep]) || X <- [Pushd | CdSeq]],
  Cmd = io_lib:format("~s~s", [CdCmd, Pwd]),
  
  Args = {args, [io_lib:format("~s \"~s\"", [COpt, Cmd])]},
  Port = open_port({spawn_executable, Shell}, [exit_status, Args]),
  receive
    {Port, {exit_status, N}}    -> 
      exit({realname, {exit_status, N}});
    {Port, {data, Data}}        ->
      receive {Port, {exit_status, 0}}  ->
        io_lib:format("~s~s~s", [trim(Data), PathSep, filename:basename(File)])
      end
  end.

os_syntax() ->
  case os:type() of
    {unix, _}   -> {"/", ";", "pwd"}; 
    {win32, _}  -> {"\\", "&", "chdir "}; % not recognized unless trailing space 
    OS          -> exit({'unknown os', OS})
  end.

%%
%% Local Functions
%%

%%@private Export to allow for hotswap.
loop(IO, State) ->
  receive
    {purging, _Pid, _Mod}							-> % chase your tail
      ?MODULE:loop(IO, State);
    {'EXIT', ExitPid, Reason}						->
      do_exit(IO, State, ExitPid, Reason);
    {stdout, Stdin, Line} when Stdin == IO#std.in	->
      do_line(IO, State, Line);
    {stderr, Stdin, Data} when Stdin == IO#std.in	->
      ?STDERR(Data);
    Noise											->
      do_noise(IO, State, Noise)
  end.

% Handle a line of input.
do_line(IO, State, Line) ->
  case Line of
    ".\n" when IO#std.stop	-> exit(ok);
    eof						-> exit(ok);
    _						-> do_procln(IO, State, Line)
  end.

% Process a line of dogo format input.
do_procln(IO, State, [$\n]) -> ?CAPTLN, ?MODULE:loop(IO, State);
do_procln(IO, State, [Space | Rest]) when Space == 32; Space == $\t ->
  {ok, MP} = re:compile("^[\\t ]*\\n$"),
  case re:run(Rest, MP, [{capture, none}]) of
    match	-> ?CAPTLN, ?MODULE:loop(IO, State); 	% ignore blank line
    nomatch -> ?STDOUT(IO), ?CAPTLN, ?MODULE:loop(IO, State)
  end;
do_procln(IO, State, [$# | _Rest]) -> ?CAPTLN, ?MODULE:loop(IO, State);
do_procln(IO, State, [$& | Rest]) ->
  File = trim(Rest),
  transclude(IO, State, File), 
  ?CAPTLN, 
  ?MODULE:loop(IO, State);
do_procln(IO, State, Line) ->
  ?STDOUT("~s~n", [trim(Line)]), ?CAPTLN, ?MODULE:loop(IO, State).

trim([$\s | String]) -> trim(String);
trim([$\t | String]) -> trim(String);
trim(String) ->
  Pattern = "^(.*[^\\s\\t\\n\\r])[\\s\\t\\n\\r]*$",
  Result = re:run(String, Pattern, [{capture, [1], list}]),
  case Result of
    nomatch				-> exit({trim, String});
    {match, [Match]}	-> Match
  end.

% Handle process exit messages.
do_exit(IO, {Cmd, _Trans, []}, ExitPid, Reason) when IO#std.in==ExitPid ->
  case Reason of
    ok			-> exit(ok);
    {ok, What}	-> exit({ok, What});
    _Else		-> exit({Cmd, Reason})
  end;
do_exit(IO, {Cmd, _Trans, _Stack}, ExitPid, Reason) when IO#std.in==ExitPid ->
  case Reason of
    ok			-> ok;
    {ok, What}	-> {ok, What};
    _Else		-> exit({Cmd, Reason})
  end;
do_exit(IO, State, _ExitPid, _Reason) -> ?MODULE:loop(IO, State).

% Handle noise on the message queue.
do_noise(IO, {Cmd, Trans, Stack}, Noise) ->
  ?STDERR("~s: noise: ~p~n", [Cmd, Noise]),
  ?CAPTLN,
  ?MODULE:loop(IO, {Cmd, Trans, Stack}).