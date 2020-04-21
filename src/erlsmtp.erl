%% The start of the erlsmtp application

-module(erlsmtp).
-behaviour(application).
-export([start/2, stop/1]).

%% Not implemented yet
start(normal, []) -> 
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    
    io:format("Welcome to ErlSMTP~n", []),    
    erlsmtp_sup:start_link().

%% Not implemented yet
stop(_) -> ok.