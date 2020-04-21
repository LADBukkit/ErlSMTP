%% The supervisor in charge of the socket acceptors for the smtp server

-module(erlsmtp_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Get Port from application environment
    {ok, Port} = application:get_env(port),
    io:format("Starting normal Server on port ~p~n", [Port]),

    %% Start server in active_once & packet_line mode
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),

    %% Spawn some listeners
    spawn_link(fun empty_listeners/0),

    {ok, {
        {simple_one_for_one, 60, 3600}, %% Restart strategy
        [{                              %% Client Specs
            socket,
            {erlsmtp_serv, start_link, [[ListenSocket, normal]]},
            temporary, 1000, worker, [erlsmtp_serv]
        }]
    }}.

start_socket() -> supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, 20)],
    ok.