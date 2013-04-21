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
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Echo words to `stdout'.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.0.5

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

-version("0.0.5").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

-import(gen_command).
-import(io).
-import(re).
-import(filename).
-import(string).
-import(file).

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2]).

% private exports
-export([loop/3, file_loop/3, run_file/4]).

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
    []			-> do_captln(IO, ARG, stdin);
    [File | _V]	-> Abs = filename:absname(File),
                   do_file_read(IO, ARG, Abs)
  end.

%%
%% Local Functions
%%

%%@private Export to allow for hotswap.
loop(IO, ARG, Src) ->
  receive
    {purging, _Pid, _Mod}							-> % chase your tail
      ?MODULE:loop(IO, ARG);
    {'EXIT', ExitPid, Reason}						->
      do_exit(IO, ARG, Src, ExitPid, Reason);
    {stdout, Stdin, Line} when Stdin == IO#std.in	->
      do_line(IO, ARG, Src, Line);
	{stderr, Stdin, Data} when Stdin == IO#std.in	->
	  ?STDERR(Data);
    Noise											->
      do_noise(IO, ARG, Src, Noise)
  end.

do_file_read(IO, ARG, Src) ->
  RunPid = spawn_link(?MODULE, run_file, [?IO(self()), ARG, ?ENV, Src]),
  NewIO = ?IO(RunPid, IO#std.out, IO#std.err, false, false),
  do_captln(NewIO, ARG, Src).

run_file(IO, ARG, _ENV, Src) ->
  case file:open(Src, [read]) of
    {error, Reason} -> exit({Src, Reason});
    {ok, Device}	-> ?MODULE:file_loop(IO, ARG, Device)
  end.

file_loop(IO, ARG, Device) ->
  receive
    {purging, _Pid, _Mod}								-> % chase your tail
      ?MODULE:file_loop(IO, ARG, Device);
    {'EXIT', ExitPid, Reason}							->
      file_loop_exit(IO, ARG, Device, ExitPid, Reason);
    {stdin, Stdout, captln} when Stdout == IO#std.out	->
      file_read_line(IO, ARG, Device);
    Noise												->
      file_noise(IO, ARG, Device, Noise)
  end.

file_loop_exit(IO, ARG, Device, ExitPid, Reason) ->
  case ExitPid of
    Stdin when Stdin == IO#std.in	->
      ?DEBUG("~s: file_loop: ~p~n", [ARG#arg.cmd, Reason]), exit(ok);
    _ 								->
      ?MODULE:file_loop(IO, ARG, Device)
  end.

file_read_line(IO, ARG, Device) ->
  case io:get_line(Device, "") of
    eof		-> file:close(Device), exit(ok);
    Line	-> ?STDOUT(Line), file_loop(IO, ARG, Device)
  end.

file_noise(IO, ARG, Device, Noise) ->
  ?STDERR("~s: noise: ~p~n", [ARG#arg.cmd, Noise]),
  file_loop(IO, ARG, Device).

% Handle a line of input.
do_line(IO, ARG, Src, Line) ->
  case Line of
    ".\n" when IO#std.stop	->
      exit(ok);
    eof						->
      exit(ok);
    _						->
      do_procln(IO, ARG, Src, Line)
  end.

% Process a line of dogo format input.
do_procln(IO, ARG, Src, [$\n]) -> do_captln(IO, ARG, Src);
do_procln(IO, ARG, Src, [Space | Rest]) when Space == 32; Space == $\t ->
  {ok, MP} = re:compile("^[\\t ]*\\n$"),
  case re:run(Rest, MP, [{capture, none}]) of
    match	-> do_captln(IO, ARG, Src); 			% ignore blank line
    nomatch -> ?STDOUT(IO), do_captln(IO, ARG, Src)
  end;
do_procln(IO, ARG, Src, [$# | _Rest]) -> do_captln(IO, ARG, Src);
do_procln(IO, ARG, Src, Line) -> ?STDOUT(Line), do_captln(IO, ARG, Src).

% Send a message to capture another line from stdin.
do_captln(IO, ARG, Src) ->
  Stdin = IO#std.in,
  Stdin ! {stdin, self(), captln},
  ?MODULE:loop(IO, ARG, Src).

% Handle process exit messages.
do_exit(IO, ARG, Src, ExitPid, Reason) ->
  case ExitPid of
    Stdin when Stdin == IO#std.in	->
	  case Reason of
		ok			-> exit(ok);
		{ok, What}	-> exit({ok, What});
		_Else		-> exit({ARG#arg.cmd, Reason})
	  end;
    _ 								->
      ?MODULE:loop(IO, ARG, Src)
  end.

% Handle noise on the message queue.
do_noise(IO, ARG, Src, Noise) ->
  ?STDERR("~s: noise: ~p~n", [ARG#arg.cmd, Noise]),
  do_captln(IO, ARG, Src).