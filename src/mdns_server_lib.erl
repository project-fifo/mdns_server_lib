-module(mdns_server_lib).

-export([start/0]).

start() ->
    application:start(ranch),
    application:start(mdns_server_lib).
