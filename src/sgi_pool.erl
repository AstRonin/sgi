-module(sgi_pool).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_link/2, once_call/2, settings/1, try_connect/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(HIBERNATE_AFTER, 1800000). % 30 minutes

-record(state, {server_name, address, port, weight, timeout, max_fails, failed_timeout, fails = 0, socket, parent, timer}).

-type tcp_active_type() :: true | false | once | (N :: -32768..32767).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name, []], []).
start_link(Name, Conf) ->
    gen_server:start_link(?MODULE, [Name, Conf], []).

once_call(Pid, Request) ->
    gen_server:call(Pid, {once, Request}).

settings(Pid) ->
    gen_server:call(Pid, get_settings).

try_connect(Pid) ->
    gen_server:call(Pid, try_connect).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Conf]) ->
    wf:info(?MODULE, "Run Pool with name: ~p~n", [Name]),
    {ok, #state{
        server_name    = pv(name, Conf, default),
        address        = pv(address, Conf, localhost),
        port           = pv(port, Conf, 80),
        weight         = pv(weight, Conf, 1),
        max_fails      = pv(max_fails, Conf, 10),
        failed_timeout = pv(failed_timeout, Conf, 1),
        timeout        = pv(timeout, Conf, 30000)
    }}.

handle_call({once, Request}, _From, State) ->
    case connect(State, false) of
        {ok, State1} ->
            ok = gen_tcp:send(State1#state.socket, Request),
            case do_recv_once(State1) of
                {ok, B} ->
                    State2 = close(State1),
                    {reply, {ok, B}, timer(State2)};
                {error, Reason} ->
                    {reply, {error, Reason}, timer(State1)}
            end;
        {error, Reason, State1} ->
            wf:error(?MODULE, "Can't create Socket: ~p~n", [Reason]),
            {reply, {error, Reason}, timer(State1)}
    end;
handle_call(get_settings, _From, State) ->
    M = #{weight => State#state.weight, server_name => State#state.server_name},
    {reply, {ok, M}, State};
handle_call(try_connect, _From, State) ->
    case connect(State) of
        {ok, State1} -> {reply, ok, State1};
        {error, _Reason, State1} -> {reply, error, State1}
    end;
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
%%                    wf:info(?MODULE, "TCP SEND to: ~p, Request: ~p~n", [State2#state.socket, Request]),
                    {noreply, timer(State2)};
                {error, Reason1} ->
                    wf:error(?MODULE, "Pool cannot send message: ~p~n", [Reason1]),
                    {noreply, timer(State2)}
            end;
        {error, Reason, State2} ->
            State2#state.parent ! {socket_error, Reason},
            wf:error(?MODULE, "Can't create Socket: ~p~n", [Reason]),
            {noreply, timer(State2)}
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

-spec connect(#state{}, tcp_active_type()) -> {ok, State :: #state{}} | {error, Reason :: term(), State :: #state{}}.
connect(State = #state{socket = undefined, address = Address, port = Port}, Active) ->
    case gen_tcp:connect(Address, Port, [binary, {active, Active}], State#state.timeout) of
        {ok, Socket} ->
            State1 = State#state{socket = Socket, fails = 0},
            wf:info(?MODULE, "TCP connect socket: ~p, Pid: ~p~n", [Socket, self()]),
            {ok, State1};
        {error, Reason} ->
            State1 = State#state{fails = State#state.fails + 1},
            State2 = overage_fail_conns(State1),
            {error, Reason, State2}
    end;
connect(State, false) ->
    State1 = close(State),
    connect(State1, false);
connect(State = #state{socket = Socket}, Active) ->
    case erlang:port_info(Socket, id) of
        undefined ->
            State1 = State#state{socket = undefined},
            connect(State1, Active);
        _ ->
            {ok, State}
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
            {error, Reason}
    end.

timer(State) ->
    Timer = timer:send_after(?HIBERNATE_AFTER, hibernate),
    State#state{timer = Timer}.

overage_fail_conns(State) ->
    case State#state.fails >= State#state.max_fails of
        true ->
            sgi_arbiter:down(self(), State#state.failed_timeout),
            State#state{fails = 0};
        _ ->
            State
    end.

pv(K, L) ->
    pv(K, L, undefined).
pv(K, L, D) ->
    proplists:get_value(K, L, D).
