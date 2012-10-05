-module(mdns_server_protocol).
-export([start_link/4, init/5]).

start_link(ListenerPid, Socket, Transport, Opts) ->
    {ok, Handler} = application:get_env(zmq_mdns_server, handler),
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Handler, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, Handler, _Opts = []) ->
    ok = ranch:accept_ack(ListenerPid),
    {ok, State} = Handler:init([]),
    loop(Handler, Socket, Transport, State).

loop(Handler, Socket, Transport, HandlerState) ->
    case Transport:recv(Socket, 0, 5000) of
	{ok, BinData} ->
	    case binary_to_term(BinData) of
		ping ->
		    Transport:send(Socket, <<"pong">>),
		    ok = Transport:close(Socket);
		Data ->
		    case Handler:message(Data, HandlerState) of
			{reply, Reply, HandlerState1} ->
			    Transport:send(Socket, term_to_binary({reply, Reply})),
			    loop(Handler, Socket, Transport, HandlerState1);
			{noreply, HandlerState1} ->
			    Transport:send(Socket, term_to_binary(noreply)),
			    loop(Handler, Socket, Transport, HandlerState1);
			{stop, _} ->
			    ok = Transport:close(Socket)
		    end
	    end;
	_ ->
	    ok = Transport:close(Socket)
    end.
