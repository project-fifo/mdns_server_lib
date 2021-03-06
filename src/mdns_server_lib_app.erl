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
    {IP, Port} = case application:get_env(mdns_server_lib, listener) of
                     {ok, R} ->
                         R;
                     _ ->
                         {ok, IPx} = application:get_env(mdns_server_lib, ip),
                         {ok, Portx} = application:get_env(
                                         mdns_server_lib, port),
                         {IPx, Portx}
                 end,
    {ok, TTL} = application:get_env(mdns_server_lib, ttl),
    MDNSConfig0 = [{port, 5353},
                   {address, {224, 0, 0, 251}},
                   {domain, Domain},
                   {type, "_" ++ Service ++ "._tcp"},
                   {ttl, TTL},
                   {options,
                    [{port, Port},
                     {ip, IP}]}],
    MDNSConfig = case application:get_env(mdns_server_lib, interface) of
                     {ok, IFace} ->
                         [{interface, IFace} | MDNSConfig0];
                     _ ->
                         MDNSConfig0
                 end,
    case application:get_env(mdns_server_lib, disabled) of
        {ok, true} ->
            ok;
        _ ->
            [A, B, C, D] = [list_to_integer(binary_to_list(P))
                            || P <- re:split(IP, "[.]")],
            {ok, _} = ranch:start_listener(
                        mdns_server, 1,
                        ranch_tcp,
                        [{port, Port}, {ip, {A, B, C, D}}],
                        mdns_server_protocol, []),
            {ok, _} = mdns_server_supervisor:start_link([MDNSConfig])
    end,
    mdns_server_lib_sup:start_link().

stop(_State) ->
    ok.
