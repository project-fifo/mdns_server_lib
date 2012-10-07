-module(mdns_server_lib_debug_handler).


-export([init/1, message/2]).

init(_Opts) ->
    {ok, stateless}.

message(Data, State) ->
    io:format("[zmq] ~p~n", [Data]),
    {reply, Data, State}.
