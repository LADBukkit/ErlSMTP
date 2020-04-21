%% Handles the socket connections

-module(erlsmtp_serv).
-behaviour(gen_server).

-record(state, {
    socket,     %% the current socket
    helo,       %% the helo state (none, helo, ehlo)
    from,       %% who send the mail
    to,         %% list of rcpts
    dataState,  %% The state of data sending (0 => not sending, 1 => sending, 2 => sent .\r\n)
    data,       %% the data
    type        %% the socket type (normal/ssl)
}).

-export([init/1, start_link/1, handle_cast/2, handle_info/2, handle_call/3]).

-define(SOCK(Msg), {tcp, _Port, Msg}).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init([Socket, Type]) ->
    %% redirect accept to server loop
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket, type=Type}}.

%% Empty handle_call
handle_call(_E, _From, State) -> {noreply, State}.

%% Accept connection
handle_cast(accept, S) ->
    {ok, AcceptSocket} = accept(S),

    %% Start new acceptor
    erlsmtp_sup:start_socket(),

    Ns = S#state{socket=AcceptSocket},

    %% Print information of connection
    {ok, {Ip, Port}} = peername(Ns),
    io:format("Accepted connection from ~p:~p~n", [Ip, Port]),

    %% Send Accept message
    {ok, Address} = application:get_env(address),
    send_ready(Ns, [Address]),
    {noreply, Ns#state{socket=AcceptSocket, helo=none, dataState=0}};

%% Empty handle_cast
handle_cast(_E, S) -> {noreply, S}.

%% Handle text after DATA
handle_info(?SOCK(Str), S = #state{dataState=Ds,data=Data}) when Ds > 0 ->
    NData = Data++Str,
    NState = case Str of
        ".\r\n" when Ds =:= 1 -> 2;
        _ -> 1
    end,
    case NState of
        2 ->
            ok(S),
            TrData = trim_data(NData),
            io:format("Data:~n~s", [TrData]),
            {noreply, S#state{from=none,to=[],data="",dataState=0}};
        _ ->
            ok = set_active(S),
            {noreply, S#state{data=NData,dataState=NState}}
    end;

%% Handle NOOP
handle_info(?SOCK("NOOP"++_), S) -> 
    ok(S),
    {noreply, S};

%% Handle QUIT
handle_info(?SOCK("QUIT"++_), S) ->
    bye(S),
    {stop, normal, S};

%% Handle RSET
handle_info(?SOCK("RSET"++_), S) ->
    ok(S),
    {noreply, S#state{from=none,to=[],data="",dataState=0}};

%% Handle HELO
handle_info(?SOCK("HELO "++Str), S = #state{helo=none}) ->
    hello(S, [line(Str)]),
    {noreply, S#state{helo=helo,from=none}};
handle_info(?SOCK("HELO "++_), S) ->
    bad_sequence(S),
    {noreply, S};

%% Handle EHLO
handle_info(?SOCK("EHLO "++_), S = #state{helo=ehlo}) ->
    bad_sequence(S),
    {noreply, S};
handle_info(?SOCK("EHLO "++Str), S) ->
    hello(S, [line(Str)]),
    {noreply, S#state{helo=ehlo,from=none}};

%% Handle MAIL FROM
handle_info(?SOCK("MAIL FROM:"++_), S = #state{helo=none}) ->
    bad_sequence(S),
    {noreply, S};
handle_info(?SOCK("MAIL FROM:"++Str), S) ->
    Sender = strip_routing(line(Str)),
    ok(S),
    {noreply, S#state{from=Sender,to=[]}};

%% Handle RCPT TO
handle_info(?SOCK("RCPT TO:"++_), S = #state{helo=none}) ->
    bad_sequence(S),
    {noreply, S};
handle_info(?SOCK("RCPT TO:"++_), S = #state{from=none}) ->
    bad_sequence(S),
    {noreply, S};
handle_info(?SOCK("RCPT TO:"++Str), S = #state{to=Rcpt}) ->
    Recv = strip_routing(line(Str)), %% TODO Check if local
    ok(S),
    {noreply, S#state{to=Rcpt++[Recv]}};

%% Handle DATA
handle_info(?SOCK("DATA:"++_), S = #state{helo=none}) ->
    bad_sequence(S),
    {noreply, S};
handle_info(?SOCK("DATA:"++_), S = #state{from=none}) ->
    bad_sequence(S),
    {noreply, S};
handle_info(?SOCK("DATA:"++_), S = #state{to=[]}) ->
    bad_sequence(S),
    {noreply, S};
handle_info(?SOCK("DATA"++_), S) ->
    start_mail(S),
    {noreply, S#state{data=[],dataState=1}};

%% Handle not implemented
handle_info(?SOCK(Str), S) ->
    {ok, {Ip, Port}} = peername(S),
    io:format("Not Implemented Command from ~p:~p -> ~s~n", [Ip, Port, line(Str)]),
    not_implemented(S),
    {noreply, S};

%% Handle closed
handle_info({tcp_closed, _Socket}, S) ->
    {stop, normal, S};

%% Handle error
handle_info({tcp_error, _Socket, _}, S) ->
    {stop, normal, S};

%% Empty handle_info
handle_info(_E, S) -> {noreply, S}.

%% Removes linebreak
line(Str) -> hd(string:tokens(Str, "\r\n")).

%% Removes routing
strip_routing(Str) -> hd(lists:reverse(string:tokens(Str, "<>,"))).

%% Removes .\r\n
trim_data(Str) -> 
    case lists:reverse(Str) of
        "\n\r."++Tri -> lists:reverse(Tri);
        _ -> Str
    end.

%% Sends a message through a socket
send(S = #state{socket=Socket,type=normal}, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str ++ "\r\n", Args)),
    ok = set_active(S),
    ok;
send(S = #state{socket=Socket,type=ssl}, Str, Args) ->
    ok = ssl:send(Socket, io_lib:format(Str ++ "\r\n", Args)),
    ok = set_active(S),
    ok.

%% Sets the socket to active once
set_active(_S = #state{socket=Socket,type=normal}) -> inet:setopts(Socket, [{active, once}]);
set_active(_S = #state{socket=Socket,type=ssl}) -> ssl:setopts(Socket, [{active, once}]).

%% Gets the peername
peername(_S = #state{socket=Socket,type=normal}) -> inet:peername(Socket);
peername(_S = #state{socket=Socket,type=ssl}) -> ssl:peername(Socket).

%% Accepts a socket
accept(_S = #state{socket=Socket,type=normal}) -> gen_tcp:accept(Socket);
accept(_S = #state{socket=Socket,type=ssl}) -> 
    {ok, TLS} = ssl:transport_accept(Socket),
    ssl:handshake(TLS).

%% MESSAGE SENDING
bye(S) -> send(S, "221 Bye", []).
send_ready(S, Args) -> send(S, "220 ~s ErlSMTP Service Ready", Args).
hello(S, Args) -> send(S, "250 Hello ~s", Args).
ok(S) -> send(S, "250 Ok", []).
start_mail(S) -> send(S, "354 Start mail input; end with <CRLF>.<CRLF>", []).
not_implemented(S) -> send(S, "502 Command not implemented", []).
bad_sequence(S) -> send(S, "503 Bad sequence of commands", []).
