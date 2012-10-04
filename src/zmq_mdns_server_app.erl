-module(zmq_mdns_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Domain} = application:get_env(zmq_mdns_server, domain),
    {ok, Service} = application:get_env(zmq_mdns_server, service),
    {ok, IP} = application:get_env(zmq_mdns_server, ip),
    {ok, Port} = application:get_env(zmq_mdns_server, port),
    {ok, Handler} = application:get_env(zmq_mdns_server, handler),
    MDNSConfig=[{port, 5353},
		{address, {224, 0, 0, 251}},
		{domain, Domain},
		{type, "_" ++ Service ++ "._zeromq._tcp"},
		{options, 
		 [{port, Port},
		  {ip, IP}]}],
    {ok, _} = ranch:start_listener(mdns_server, 1,
				   ranch_tcp, [{port, Port}], mdns_server_protocol, []),
    {ok, _} = mdns_server_supervisor:start_link([MDNSConfig]),
    zmq_mdns_server_sup:start_link(Handler).

stop(_State) ->
    ok.
