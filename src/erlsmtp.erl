%% The start of the erlsmtp application

-module(erlsmtp).
-behaviour(application).
-export([start/2, stop/1]).

%% Not implemented yet
start(normal, []) -> erlsmtp_sup:start_link().

%% Not implemented yet
stop(_) -> ok.