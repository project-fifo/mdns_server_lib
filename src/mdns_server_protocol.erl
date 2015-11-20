-module(mdns_server_protocol).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([start_link/4,
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/4]).

-record(state, {socket,
                transport,
                ok,
                error,
                closed,
                claim = false,
                handler,
                handler_state}).


start_link(ListenerPid, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init,
                        [[ListenerPid, Socket, Transport, Opts]]).

init([ListenerPid, Socket, Transport, _Opts = []]) ->
    {ok, Handler} = application:get_env(mdns_server_lib, handler),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(ListenerPid),
    ok = Transport:setopts(Socket, [{active, true}, {packet, 4}]),
    {OK, Closed, Error} = Transport:messages(),
    {ok, State} = Handler:init(self(), []),
    gen_server:enter_loop(?MODULE, [], #state{
                                          handler_state = State,
                                          ok = OK,
                                          closed = Closed,
                                          error = Error,
                                          socket = Socket,
                                          handler = Handler,
                                          transport = Transport}).

handle_info({_Closed, _Socket}, State = #state{
                                           handler = Handler,
                                           handler_state = HandlerState,
                                           closed = _Closed}) ->
    case erlang:function_exported(Handler, close, 1) of
        false ->
            ok;
        true ->
            Handler:close(HandlerState)
    end,
    {stop, normal, State};

handle_info({data, Data}, State = #state{socket = Socket,
                                         transport = Transport}) ->
    Transport:send(Socket, term_to_binary(Data)),
    {noreply, State};

handle_info(Info, State = #state{
                             handler_state = HandlerState,
                             handler = Handler,
                             claim = true}) ->
    case Handler:raw(Info, HandlerState) of
        {ok, HandlerState1} ->
            {noreply, State#state{handler_state = HandlerState1}};
        {stop, Reason, HandlerState1} ->
            {stop, Reason, State#state{handler_state = HandlerState1}}
    end;

handle_info({_OK, Socket, BinData}, State = #state{
                                               claim = false,
                                               ok = _OK}) ->
    handle_data(binary_to_term(BinData), Socket, State);

handle_info(Info, State) ->
    lager:warning("[mdns server] Unknown message: ~p ",
                  [Info]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknwon}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_data(ping, Socket, State = #state{transport = Transport}) ->
    Transport:send(Socket, term_to_binary(pong)),
    {noreply, State};

handle_data({trace, Token = [], Data}, Socket, State) ->
    seq_trace:set_token(Token),
    handle_data(Data, Socket, State);

handle_data({trace, Token, Data}, Socket, State) ->
    seq_trace:set_token(Token),
    handle_data(Data, Socket, State);


handle_data(Data, Socket, State = #state{
                                     handler = Handler,
                                     handler_state = HandlerState,
                                     transport = Transport}) ->
    case Handler:message(Data, HandlerState) of
        {claim, HandlerState1} ->
            Transport:send(Socket, term_to_binary(noreply)),
            {noreply, State#state{claim = true,
                                  handler_state = HandlerState1}};
        {reply, Reply, HandlerState1} ->
            Transport:send(Socket, term_to_binary({reply, Reply})),
            {noreply, State#state{handler_state = HandlerState1}};
        {noreply, HandlerState1} ->
            Transport:send(Socket, term_to_binary(noreply)),
            {noreply, State#state{handler_state = HandlerState1}};
        {stop, normal, Reply, HandlerState1} ->
            Transport:send(Socket, term_to_binary({reply, Reply})),
            ok = Transport:close(Socket),
            {stop, normal, State#state{handler_state = HandlerState1}};
        {stop, normal, HandlerState1} ->
            Transport:send(Socket, term_to_binary(noreply)),
            ok = Transport:close(Socket),
            {stop, normal, State#state{handler_state = HandlerState1}};
        {stop, Reason, Reply, HandlerState1} ->
            Transport:send(Socket, term_to_binary({reply, Reply})),
            lager:warning("[mdns server] Abnormal Stop(~p), Reply: ~p "
                          "/ handler state was: ~p ",
                          [Reason, Reply, HandlerState1]),
            ok = Transport:close(Socket),
            {stop, Reason, State#state{handler_state = HandlerState1}};
        {stop, Reason, HandlerState1} ->
            Transport:send(Socket, term_to_binary(noreply)),
            lager:warning("[mdns server] Abnormal Stop(~p), handler state was: "
                          "~p", [Reason, HandlerState1]),
            ok = Transport:close(Socket),
            {stop, Reason, State#state{handler_state = HandlerState1}}
    end.
