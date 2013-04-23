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
-import(re).
-import(pose_open).

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
				   ?MODULE:loop(IO, ARG);
    [File | _V]	-> ReadPid = pose_open:read(File),
                   NewIO = ?IO(ReadPid, IO#std.out, IO#std.err),
                   ?CAPTLN(ReadPid),
				   ?MODULE:loop(NewIO, ARG)
  end.

%%
%% Local Functions
%%

%%@private Export to allow for hotswap.
loop(IO, ARG) ->
  receive
    {purging, _Pid, _Mod}							-> % chase your tail
      ?MODULE:loop(IO, ARG);
    {'EXIT', ExitPid, Reason}						->
      do_exit(IO, ARG, ExitPid, Reason);
    {stdout, Stdin, Line} when Stdin == IO#std.in	->
      do_line(IO, ARG, Line);
    {stderr, Stdin, Data} when Stdin == IO#std.in	->
      ?STDERR(Data);
    Noise											->
      do_noise(IO, ARG, Noise)
  end.

% Handle a line of input.
do_line(IO, ARG, Line) ->
  case Line of
    ".\n" when IO#std.stop	-> exit(ok);
    eof						-> exit(ok);
    _						-> do_procln(IO, ARG, Line)
  end.

% Process a line of dogo format input.
do_procln(IO, ARG, [$\n]) -> ?CAPTLN, ?MODULE:loop(IO, ARG);
do_procln(IO, ARG, [Space | Rest]) when Space == 32; Space == $\t ->
  {ok, MP} = re:compile("^[\\t ]*\\n$"),
  case re:run(Rest, MP, [{capture, none}]) of
    match	-> ?CAPTLN, ?MODULE:loop(IO, ARG); 	% ignore blank line
    nomatch -> ?STDOUT(IO), ?CAPTLN, ?MODULE:loop(IO, ARG)
  end;
do_procln(IO, ARG, [$# | _Rest]) -> ?CAPTLN, ?MODULE:loop(IO, ARG);
do_procln(IO, ARG, Line) -> 
  ?STDOUT(Line), ?CAPTLN, ?MODULE:loop(IO, ARG).

% Handle process exit messages.
do_exit(IO, ARG, ExitPid, Reason) ->
  case ExitPid of
    Stdin when Stdin == IO#std.in	->
      case Reason of
        ok			-> exit(ok);
        {ok, What}	-> exit({ok, What});
        _Else		-> exit({ARG#arg.cmd, Reason})
      end;
    _ 								->
      ?MODULE:loop(IO, ARG)
  end.

% Handle noise on the message queue.
do_noise(IO, ARG, Noise) ->
  ?STDERR("~s: noise: ~p~n", [ARG#arg.cmd, Noise]),
  ?CAPTLN,
  ?MODULE:loop(IO, ARG).