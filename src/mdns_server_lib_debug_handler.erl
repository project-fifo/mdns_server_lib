-module(mdns_server_lib_debug_handler).

-export([init/1, message/2]).

%%--------------------------------------------------------------------
%% @doc This function is called when the handler is initialized.
%%  It will return a new state used by the other callbacks.
%% @spec init(Opts::any()) ->
%%    {ok, State::any()} |
%%    {ok, stateless}
%% @end
%%--------------------------------------------------------------------

%-sepc init(any()) ->
%    {ok, State::any()} |
%    {ok, stateless}.
init(_Opts) ->
    {ok, stateless}.

%%--------------------------------------------------------------------
%% @doc message will be called whenever a mesage was received.
%%  The data can be handed on to your own code and either return a
%%  value by using `{reply, Reply, NewState}' or just let the call 
%%  return without a reply by returning `{noreply, NewState}'. If an
%%  error occurs you can use `{stop, NewState}' which will close the
%%  connection without a reply at all which.
%% @spec message(Data::term(), State::term()) ->
%%		     {reply, Reply::term(), NewState::term()}
%%			 | {noreply, NewState::term()}
%%			 | {stop, NewState::term()}
%% @end
%%--------------------------------------------------------------------

-spec message(Data::term(), State::term()) ->
		     {reply, Reply::term(), NewState::term()} |
		     {noreply, NewState::term()} |
		     {stop, NewState::term()}.

message(Data, State) ->
    io:format("[zmq] ~p~n", [Data]),
    {reply, Data, State}.
