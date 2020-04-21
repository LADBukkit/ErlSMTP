%% Handles the socket connections

-module(erlsmtp_serv).
-behaviour(gen_server).

-record(state, {
    socket,     %% the current socket
    helo,       %% the helo state (none, helo, ehlo)
    from,       %% who send the mail
    to,         %% list of rcpts
    dataState,  %% The state of data sending (0 => not sending, 1 => sending, 2 => sent .\r\n)
    data        %% the data
}).

-export([init/1, start_link/1, handle_cast/2, handle_info/2, handle_call/3]).

-define(SOCK(Msg), {tcp, _Port, Msg}).

start_link(Socket) -> gen_server:start_link(?MODULE, Socket, []).

init(Socket) -> 
    %% redirect accept to server loop
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

%% Empty handle_call
handle_call(_E, _From, State) -> {noreply, State}.

%% Accept connection
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %% Start new acceptor
    erlsmtp_sup:start_socket(),
    %% Print information of connection
    {ok, {Ip, Port}} = inet:peername(AcceptSocket),
    io:format("Accepted connection from ~p:~p~n", [Ip, Port]),
    %% Send Accept message
    {ok, Address} = application:get_env(address),
    send(AcceptSocket, "220 ~s ErlSMTP Service Ready", [Address]),
    {noreply, S#state{socket=AcceptSocket, helo=none, dataState=0}};
%% Empty handle_cast
handle_cast(_E, S) -> {noreply, S}.

%% Handle text after DATA
handle_info(?SOCK(Str), S = #state{socket=Socket,dataState=Ds,data=Data}) when Ds > 0 ->
    NData = Data++Str,
    NState = case Str of
        ".\r\n" when Ds =:= 1 -> 2;
        _ -> 1
    end,
    case NState of
        2 ->
            ok(Socket),
            TrData = trim_data(NData),
            io:format("Data:~n~s", [TrData]),
            {noreply, S#state{from=none,to=[],data=""}};
        _ ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, S#state{data=NData,dataState=NState}}
    end;
%% Handle NOOP
handle_info(?SOCK("NOOP"++_), S = #state{socket=Socket}) -> 
    ok(Socket),
    {noreply, S};
%% Handle QUIT
handle_info(?SOCK("QUIT"++_), S = #state{socket=Socket}) ->
    bye(Socket),
    {stop, normal, S};
%% Handle RSET
handle_info(?SOCK("RSET"++_), S = #state{socket=Socket}) ->
    ok(Socket),
    {noreply, S#state{from=none,to=[],data=""}};
%% Handle HELO
handle_info(?SOCK("HELO "++Str), S = #state{socket=Socket, helo=none}) ->
    send(Socket, "250 Hello ~s", [line(Str)]),
    {noreply, S#state{helo=helo,from=none}};
handle_info(?SOCK("HELO "++_), S = #state{socket=Socket}) ->
    bad_sequence(Socket),
    {noreply, S};
%% Handle MAIL FROM
handle_info(?SOCK("MAIL FROM:"++_), S = #state{socket=Socket, helo=none}) ->
    bad_sequence(Socket),
    {noreply, S};
handle_info(?SOCK("MAIL FROM:"++Str), S = #state{socket=Socket}) ->
    Sender = strip_routing(line(Str)),
    ok(Socket),
    {noreply, S#state{from=Sender,to=[]}};
%% Handle RCPT TO
handle_info(?SOCK("RCPT TO:"++_), S = #state{socket=Socket, helo=none}) ->
    bad_sequence(Socket),
    {noreply, S};
handle_info(?SOCK("RCPT TO:"++_), S = #state{socket=Socket, from=none}) ->
    bad_sequence(Socket),
    {noreply, S};
handle_info(?SOCK("RCPT TO:"++Str), S = #state{socket=Socket, to=Rcpt}) ->
    Recv = strip_routing(line(Str)), %% TODO Check if local
    ok(Socket),
    {noreply, S#state{to=Rcpt++[Recv]}};
%% Handle DATA
handle_info(?SOCK("DATA:"++_), S = #state{socket=Socket, helo=none}) ->
    bad_sequence(Socket),
    {noreply, S};
handle_info(?SOCK("DATA:"++_), S = #state{socket=Socket, from=none}) ->
    bad_sequence(Socket),
    {noreply, S};
handle_info(?SOCK("DATA:"++_), S = #state{socket=Socket, to=[]}) ->
    bad_sequence(Socket),
    {noreply, S};
handle_info(?SOCK("DATA"++_), S = #state{socket=Socket}) ->
    start_mail(Socket),
    {noreply, S#state{data=[],dataState=1}};
%% Handle not implemented
handle_info(?SOCK(Str), S = #state{socket=Socket}) ->
    {ok, {Ip, Port}} = inet:peername(Socket),
    io:format("Not Implemented Command from ~p:~p -> ~s~n", [Ip, Port, line(Str)]),
    not_implemented(Socket),
    {noreply, S};
%% Handle closed
handle_info({tcp_closed, _Socket}, S) ->
    {stop, normal, S};
%% Handle error
handle_info({tcp_error, _Socket, _}, S) ->
    {stop, normal, S};
%% Empty handle_info
handle_info(_E, S) -> {noreply, S}.

%% Sends a message through a socket
send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str ++ "\r\n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

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


%% MESSAGE SENDING
bye(Socket) -> send(Socket, "221 Bye", []).
ok(Socket) -> send(Socket, "250 Ok", []).
start_mail(Socket) -> send(Socket, "354 Start mail input; end with <CRLF>.<CRLF>", []).
not_implemented(Socket) -> send(Socket, "502 Command not implemented", []).
bad_sequence(Socket) -> send(Socket, "503 Bad sequence of commands", []).
