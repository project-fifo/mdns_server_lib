-module(mdns_server_lib_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Domain} = application:get_env(mdns_server_lib, domain),
    {ok, Service} = application:get_env(mdns_server_lib, service),
    {ok, IP} = application:get_env(mdns_server_lib, ip),
    {ok, Port} = application:get_env(mdns_server_lib, port),
    MDNSConfig=[{port, 5353},
                {address, {224, 0, 0, 251}},
                {domain, Domain},
                {type, "_" ++ Service ++ "._tcp"},
                {options,
                 [{port, Port},
                  {ip, IP}]}],
    {ok, _} = ranch:start_listener(mdns_server, 1,
                                   ranch_tcp, [{port, Port}], mdns_server_protocol, []),
    {ok, _} = mdns_server_supervisor:start_link([MDNSConfig]),
    mdns_server_lib_sup:start_link().

stop(_State) ->
    ok.
