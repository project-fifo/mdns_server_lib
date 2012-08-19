
-module(zmq_mdns_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(CHILD(I, Type, P), {I, {I, start_link, P}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Handler) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Handler]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Handler]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(zmq_mdns_server_fsm, worker, [Handler])]}}.
