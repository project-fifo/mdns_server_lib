-module(zmq_mdns_server).

-export([start/0]).

start() ->
    application:start(ranch),
    application:start(zmq_mdns_server).
