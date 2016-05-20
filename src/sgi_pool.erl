-module(sgi_pool).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {address, port, socket, parent}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.




handle_info({send, Request, From}, State) ->
    State1 = State#state{parent = From},
    case connect(State1) of
        {ok, State2} ->
%%            gen_tcp:send(State2#state.socket, Request);
            case gen_tcp:send(State2#state.socket, Request) of
                ok ->
                    {noreply, State2};
                {error, Reason1} ->
                    wf:error(?MODULE, "Pool cannot send message: ~p~n", [Reason1]),
                    {noreply, State2}
            end;
        {error, Reason} ->
            wf:error(?MODULE, "Can't create Socket: ~p~n", [Reason]),
            {noreply, State1}
    end
%%    {noreply, State}
;
handle_info({tcp, Socket, Data}, State) ->
    State#state.parent ! {socket_return, Data},
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    State1 = State#state{socket = undefined},
    {noreply, State1};
handle_info({tcp_error, _Socket, _Reason}, State) ->
    State1 = State#state{socket = undefined},
    {noreply, State1};
handle_info(Info, State) ->
    wf:error(?MODULE, "Unexpected message: ~p~n", {Info}),
    {noreply, State}.


terminate(_Reason, State) ->
    close_connetion(State),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


connect(State = #state{socket = undefined, address = Address, port = Port}) ->
    case gen_tcp:connect(Address, Port, [binary, {active, once}]) of
        {ok, Socket} ->
            State1 = State#state{socket = Socket},
            {ok, State1};
        {error, Reason} ->
            {error, Reason}
    end;
connect(State = #state{socket = Socket}) ->
    case inet:getstat(Socket) of
        {ok, _} -> {ok, State};
        {error, _} ->
            State1 = State#state{socket = undefined},
            connect(State1)
    end.

close_connetion(State) ->
    gen_tcp:close(State#state.socket).
