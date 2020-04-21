%% The start of the erlsmtp application

-module(erlsmtp).
-behaviour(application).
-export([start/2, stop/1]).

%% Not implemented yet
start(normal, []) -> 
    io:format("Welcome to ErlSMTP~n", []),    
    erlsmtp_sup:start_link().
    %%erlsmtp_sup_ssl:start_link(),

%% Not implemented yet
stop(_) -> ok.