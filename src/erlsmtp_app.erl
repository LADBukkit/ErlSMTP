-module(erlsmtp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    
    io:format("Welcome to ErlSMTP~n", []),  
    erlsmtp_sup:start_link().

stop(_State) ->
    ok.

