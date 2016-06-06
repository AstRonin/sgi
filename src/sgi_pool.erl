-module(sgi_pool).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, once_call/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(HIBERNATE_AFTER, 1800000). % 30 minutes

-record(state, {address, port, socket, parent, timer}).

-type tcp_active_type() :: true | false | once | (N :: -32768..32767).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

once_call(Pid, Request) ->
    gen_server:call(Pid, {once, Request}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name]) ->
    wf:info(?MODULE, "Run Pool with name: ~p~n", [Name]),
    {ok, #state{address = wf:config(sgi, address, localhost), port = wf:config(sgi, port, 80)}}.

handle_call({once, Request}, _From, State) ->
    case connect(State, false) of
        {ok, State1} ->
            ok = gen_tcp:send(State1#state.socket, Request),
            {ok, B} = do_recv_once(State1),
            State2 = close(State1),
            {reply, {ok, B}, timer(State2)};
        {error, Reason} ->
            wf:error(?MODULE, "Can't create Socket: ~p~n", [Reason]),
            {reply, {error, Reason}, timer(State)}
    end. %% ;
%%handle_call(_Request, _From, State) ->
%%    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({send, Request, From}, State) ->
    State1 = State#state{parent = From},
    case connect(State1) of
        {ok, State2} ->
%%            gen_tcp:send(State2#state.socket, Request);
            case gen_tcp:send(State2#state.socket, Request) of
                ok ->
%%                    wf:info(?MODULE, "TCP SEND to: ~p, Request: ~p~n", [State2#state.socket, Request]),
                    {noreply, timer(State2)};
                {error, Reason1} ->
                    wf:error(?MODULE, "Pool cannot send message: ~p~n", [Reason1]),
                    {noreply, timer(State2)}
            end;
        {error, Reason} ->
            wf:error(?MODULE, "Can't create Socket: ~p~n", [Reason]),
            {noreply, timer(State1)}
    end;
handle_info({tcp, Socket, Data}, State) ->
    State#state.parent ! {socket_return, Data},
    inet:setopts(Socket, [{active, once}]),
    {noreply, timer(State)};
handle_info({tcp_closed, _Socket}, State) ->
    wf:info(?MODULE, "TCP connection CLOSED with state: ~p~n", [State]),
    {noreply, timer(State#state{socket = undefined})};
handle_info({tcp_error, _Socket, Reason}, State) ->
    wf:info(?MODULE, "TCP connection got ERROR: ~p with state: ~p~n", [Reason, State]),
    {noreply, timer(State#state{socket = undefined})};
handle_info(hibernate, State) ->
    State1 = close(State),
    {noreply, State1, hibernate};
handle_info(Info, State) ->
    wf:error(?MODULE, "Unexpected message: ~p~n", [Info]),
    {noreply, timer(State)}.

terminate(_Reason, State) ->
    close(State),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(State) -> connect(State, once).

-spec connect(#state{}, tcp_active_type()) -> {ok, State :: #state{}} | {error, Reason :: term()}.
connect(State = #state{socket = undefined, address = Address, port = Port}, Active) ->
    case gen_tcp:connect(Address, Port, [binary, {active, Active}], wf:config(sgi, timeout, 30000)) of
        {ok, Socket} ->
            State1 = State#state{socket = Socket},
            wf:info(?MODULE, "TCP connect socket: ~p~n", [Socket]),
            {ok, State1};
        {error, Reason} ->
            {error, Reason}
    end;
connect(State = #state{socket = Socket}, Active) ->
    case erlang:port_info(Socket, id) of
        undefined -> State1 = State#state{socket = undefined},
            connect(State1, Active);
        _ -> {ok, State}
    end.

close(State) when State#state.socket /= undefined ->
    gen_tcp:close(State#state.socket),
    State#state{socket = undefined};
close(State) -> State.

do_recv_once(State) ->
    case gen_tcp:recv(State#state.socket, 0) of
        {ok, B} ->
            {ok, B};
        {error, Reason} ->
            wf:error(?MODULE, "do_recv_once: ~p~n", {Reason}),
            {error, Reason}
    end.

timer(State) ->
    Timer = timer:send_after(?HIBERNATE_AFTER, hibernate),
    State#state{timer = Timer}.
