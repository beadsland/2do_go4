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

%% @version 0.0.8

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

-version("0.0.8").

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
-import(pose_file).

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
%% Callback Function
%%

%% @private Callback entry point for gen_command behaviour.
do_run(IO, ARG) ->
  Format = "Starting Dogo ~s list interpreter ~p~n",
  ?STDOUT(Format, [?VERSION(?MODULE), self()]),
  case do_run(IO, ARG, ?ARGV(1)) of
    ok              -> exit(ok);
    {error, Reason} -> exit({ARG#arg.cmd, Reason})
  end.

do_run(IO, _ARG, undefined) -> transclude(IO, [stdin]);
do_run(IO, _ARG, File) -> 
  Canon = pose_file:realname(File), 
  transclude(IO, [{Canon, File}]).

%%
%% Local Functions
%%

%%@private Export to allow for hotswap.
loop(IO, Trans) ->
  receive
    {purging, _Pid, _Mod}							-> % chase your tail
      ?MODULE:loop(IO, Trans);
    {'EXIT', ExitPid, Reason}						->
      do_exit(IO, Trans, ExitPid, Reason);
    {stdout, Stdin, Line} when Stdin == IO#std.in	->
      do_line(IO, Trans, Line);
    {stderr, Stdin, Data} when Stdin == IO#std.in	->
      ?STDERR(Data);
    Noise											->
      do_noise(IO, Trans, Noise)
  end.

% Handle a line of input.
do_line(IO, Trans, Line) ->
  case Line of
    ".\n" when IO#std.stop	-> ok;
    eof						-> ok;
    _						-> do_procln(IO, Trans, Line)
  end.

% Process a line of dogo format input.
do_procln(IO, Trans, [$\n]) -> ?CAPTLN, ?MODULE:loop(IO, Trans);
do_procln(IO, Trans, [$# | _Rest]) -> ?CAPTLN, ?MODULE:loop(IO, Trans);
do_procln(IO, Trans, [$\s | Rest]) -> do_procln(IO, Trans, Rest);
do_procln(IO, Trans, [$\t | Rest]) -> do_procln(IO, Trans, Rest);
do_procln(IO, [stdin], [$& | Rest]) ->
  NewFile = pose_file:trim(Rest),
  NewCanon = pose_file:realname(NewFile),
  NewTrans = [{NewCanon, NewFile} | [stdin]],
  do_transln(IO, NewTrans);
do_procln(IO, Trans, [$& | Rest]) ->
  [{Canon, _File} | _Trans] = Trans,
  NewFile = pose_file:trim(Rest),
  NewCanon = pose_file:realname(NewFile, Canon),
  NewTrans = [{NewCanon, NewFile} | Trans],
  do_transln(IO, NewTrans);
do_procln(IO, Trans, Line) ->
  ?STDOUT("~s~n", [pose_file:trim(Line)]), 
  ?CAPTLN, ?MODULE:loop(IO, Trans).

% Process a transclude line from dogo input.
do_transln(IO, Trans) ->
  case transclude(IO, Trans) of
    ok              -> ?CAPTLN, ?MODULE:loop(IO, Trans);
    {error, Reason} -> {error, Reason}
  end.

% Open new file process, and swap it in as new stdin.
transclude(IO, [stdin]) -> ?CAPTLN, ?MODULE:loop(IO, [stdin]);
transclude(IO, [{Canon, File} | Trans]) ->
  ReadPid = pose_open:read(Canon),
  NewIO = ?IO(ReadPid, IO#std.out, IO#std.err),
  ?CAPTLN(ReadPid),
  ?MODULE:loop(NewIO, [{Canon, File} | Trans]).

% Handle process exit messages.
do_exit(IO, _Trans, ExitPid, Reason) when ExitPid==IO#std.in -> Reason;
do_exit(IO, Trans, _ExitPid, _Reason) -> ?MODULE:loop(IO, Trans).

% Handle noise on the message queue.
do_noise(IO, Trans, Noise) ->
  ?STDERR("~s: noise: ~p~n", [get(command), Noise]),
  ?CAPTLN, ?MODULE:loop(IO, Trans).