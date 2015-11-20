-module(mdns_server_lib).

-export([start/0]).

-spec start() -> ok |
                 {error, Reason::any()}.
start() ->
    application:start(ranch),
    application:start(mdns_server_lib).
