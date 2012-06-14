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

%% @doc Report information about active processes.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.0

-define(module, ps).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.1.0").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

-import(gen_command).
-import(erlang).
-import(proplists).
-import(io_lib).
-import(string).
-import(lists).

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2]).

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
do_run(IO, _ARG) ->
    Processes = erlang:processes(),
    Formats = [{pid, "~11s"}, {memory_kilo, "~5s"},
                              {messages, "~5s"},
                              {current, "~15s"},
                              {command, "~s"}
              ],
    Headers = [{pid, "PID"}, {memory_kilo, "RES"},
                             {messages, "MSGS"},
                             {current, "CURRENT"},
                             {command, "COMMAND"}
              ],
    ProcInfo = [get_info(X) || X <- Processes],
    Format = string:join([erlang:element(2, X) || X <- Formats], "  "),
    Keys = [erlang:element(1, X) || X <- Headers],

    print_out(IO, Format ++ "~n", Keys, [Headers | ProcInfo]).

%%
%% Local Functions
%%

print_out(_IO, _Format, _Keys, []) -> ok;
print_out(IO, Format, Keys, [PropList | Tail]) ->
  Values = [proplists:get_value(X, PropList) || X <- Keys],
  ?STDOUT(Format, Values),
  print_out(IO, Format, Keys, Tail).

get_info(Pid) ->
  ItemSpec = [current_function, memory, message_queue_len, dictionary],
  Info = erlang:process_info(Pid, ItemSpec),
  stringify_info([{pid, Pid} | Info]).

-define(GET(X), proplists:get_value(X, InfoList)).
-define(P(X), io_lib:format("~p", [X])).

stringify_info(InfoList) ->
  Memory = proplists:get_value(memory, InfoList),
  StringList = [{memory_kilo, kilo_value(Memory)},
                {messages, ?P(?GET(message_queue_len))},
                {pid, ?P(?GET(pid))},
                {current, erlang:element(1, ?GET(current_function))},
                {command, proplists:get_value(command, ?GET(dictionary))}
               ],
  lists:append(StringList, InfoList).

kilo_value(Value) when Value < 1024*1024 ->
  io_lib:format("~p", [erlang:round(Value / 1024)]);
kilo_value(Value) ->
  io_lib:format("~pm", [erlang:round(Value / 1024 / 1024 *10) / 10]).