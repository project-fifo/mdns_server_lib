-module(zmq_mdns_server_debug_handler).


-export([init/1, message/2]).

init(_Opts) ->
    {ok, stateless}.

message(Data, State) ->
    io:format("[zmq] ~p~n", [Data]),
    {reply, Data, State}.
