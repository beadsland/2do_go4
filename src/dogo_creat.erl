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

%% @doc A `pose'-command interface for reading and writing files in 
%% local filesystem.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo
 
%% @version 0.0.1

-define(module, dogo_creat).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.0.1").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

-import(gen_command).
-import(file).
-import(lists).
-import(io).

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2]).

% file entry points
-export([open_read/1]).

% private exports
-export([loop/3]).

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
  File = ?ARGV(1), Mode = ?ARGV(2),
  IsRead = lists:member(read, Mode),
  IsWrite = lists:member(write, Mode),
  case file:open(File, Mode) of
    {error, Reason} -> exit({File, Reason});
    {ok, Device}	-> ?MODULE:loop(IO, Device, {IsRead, IsWrite})
  end.

%%
%% File Entry Points
%%

-type filename() :: file:name_all() | iodata().
-type read_pid() :: pid().

-spec open_read(File :: filename()) -> read_pid().
open_read(File) ->
  ARG = ?ARG(?MODULE, [File, [read]]),
  spawn_link(?MODULE, run, [?IO(self()), ARG, ?ENV]).
  
%%
%% Local Functions
%%

%%@private Export to allow for hotswap.
loop(IO, Device, Access) ->
  {R, _W} = Access,
  receive
    {purging, _Pid, _Mod}									-> 
      ?MODULE:loop(IO, Device, Access);					% chase your tail
    {'EXIT', ExitPid, Reason}								->
      do_exit(IO, Device, Access, ExitPid, Reason);
    {stdin, Stdout, captln} when R, Stdout == IO#std.out	->
      do_readln(IO, Device, Access);
%    {stdout, Stdin, Line} when W, Stdin == IO#std.in		->
%      do_writeln(IO, Device, Access);
    Noise													->
      do_noise(IO, Device, Access, Noise)
  end.

do_readln(IO, Device, Access) ->
  case io:get_line(Device, "") of
    eof		-> file:close(Device), exit(ok);
    Line	-> ?STDOUT(Line), ?MODULE:loop(IO, Device, Access)
  end.

do_exit(IO, Device, {R, W}, ExitPid, Reason) ->
  case ExitPid of
    Stdout when R, Stdout == IO#std.out	->
      ?DEBUG("reader: ~p~n", [Reason]), exit(ok);
    Stdin when W, Stdin == IO#std.in	->
      ?DEBUG("writer: ~p~n", [Reason]), exit(ok);
    _ 									->
      ?MODULE:loop(IO, Device, {R, W})
  end.

do_noise(IO, Device, Access, Noise) ->
  ?STDERR("noise: ~p~n", [Noise]),
  ?MODULE:loop(IO, Device, Access).