-module(zmq_mdns_server).

-export([start/0]).

start() ->
    application:start(zmq_mdns_server).
