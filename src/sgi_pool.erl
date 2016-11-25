-module(sgi_pool).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_link/2, once_call/1, once_call/2, settings/1, try_connect/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(ARBITER, sgi_arbiter).
-define(HIBERNATE_AFTER, 1800000). % 30 minutes

-record(state, {
    server_name,
    address,
    port,
    weight,
    timeout,
    max_fails,
    failed_timeout,
    fails = 0,
    socket,
    from,
    timer,
    last_active_time = 0
}).

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

%%
%% Simple and small request.
%% Send request to a server, receive only one response, and free a socket
%%
-spec once_call(binary()) -> {ok, binary()} | {error, term()}.
once_call(Request) ->
    case ?ARBITER:alloc() of
        {ok, PoolPid} ->
            Ret = try
                gen_server:call(PoolPid, {once, Request}, 600000)
            catch
                _:Reason ->
                    {error, Reason}
            end,
            ?ARBITER:free(PoolPid),
            Ret;
        {error, Reason} ->
            {error, Reason}
    end.

-spec once_call(pid(), binary()) -> {ok, binary()} | {error, term()}.
once_call(PoolPid, Request) ->
    try
        gen_server:call(PoolPid, {once, Request}, 600000)
    catch
        _:Reason ->
            wf:error(?MODULE, "Gen Server Call failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%%
%% Getting settings of this connection for control
%%
settings(Pid) ->
    gen_server:call(Pid, get_settings).

%%
%% Checking connect to a server
%%
try_connect(Pid) ->
    gen_server:call(Pid, try_connect).

%%-spec jsend(binary()) -> {ok, binary()} | {error, term()}.
%%jsend(Request) ->
%%    {ok, PoolPid} = ?ARBITER:alloc(),
%%    Ret = gen_server:call(PoolPid, {jsend, Request}),
%%    ?ARBITER:free(PoolPid),
%%    Ret.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([_Name, Conf]) ->
    timer:send_interval(?HIBERNATE_AFTER, self(), hibernate),
    {ok, #state{
        server_name    = sgi:pv(name, Conf, default),
        address        = sgi:pv(address, Conf, localhost),
        port           = sgi:pv(port, Conf, 9000),
        weight         = sgi:pv(weight, Conf, 1),
        max_fails      = sgi:pv(max_fails, Conf, 10),
        failed_timeout = sgi:pv(failed_timeout, Conf, 1), % in seconds
        timeout        = sgi:pv(timeout, Conf, 30000)
    }}.

handle_call({once, Request}, _From, State) ->
    case connect(State, false) of
        {ok, State1} ->
            ok = gen_tcp:send(State1#state.socket, Request),
            case do_recv_once(State1) of
                {ok, B} ->
                    {reply, {ok, B}, set_last_active_time(close(State1))};
                {error, Reason} ->
                    {reply, {error, Reason}, set_last_active_time(close(State1))}
            end;
        {error, Reason, State1} ->
            wf:error(?MODULE, "Can't create Socket: ~p~n", [Reason]),
            {reply, {error, Reason}, set_last_active_time(State1)}
    end;
handle_call(get_settings, _From, State) ->
    M = #{weight => State#state.weight, server_name => State#state.server_name},
    {reply, {ok, M}, State};
handle_call(try_connect, _From, State) ->
    case connect(State) of
        {ok, State1} -> {reply, ok, State1};
        {error, _Reason, State1} -> {reply, error, State1}
    end;
%%handle_call({jsend, Request}, _From, State) ->
%%    {ok, State1} = connect(State, false),
%%    ok = gen_tcp:send(State1#state.socket, Request),
%%    Ret = do_recv(State1, []),
%%    State2 = close(State1),
%%    {reply, Ret, State2};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
%%
%% Send msg
%%
handle_info({send, Request, From}, State) ->
    State1 = State#state{from = From},
    case connect(State1) of
        {ok, State2} ->
%%            wf:info(?MODULE, "Send data from pool to uwsgi: ~p~n", [Request]),
            case gen_tcp:send(State2#state.socket, Request) of
                ok ->
                    {noreply, set_last_active_time(State2)};
                {error, Reason1} ->
                    State2#state.from ! {socket_error, Reason1},
                    wf:error(?MODULE, "Pool cannot send message: ~p~n", [Reason1]),
                    {noreply, set_last_active_time(State2)}
            end;
        {error, Reason, State2} ->
            State2#state.from ! {socket_error, Reason},
            wf:error(?MODULE, "Can't create Socket: ~p~n", [Reason]),
            {noreply, set_last_active_time(State2)}
    end;

%%
%% Receive msg
%%
handle_info({tcp, Socket, Data}, State) ->
    State#state.from ! {socket_return, Data},
    inet:setopts(Socket, [{active, once}]),
    {noreply, set_last_active_time(State)};

%%
%% Receive msg about connection was closed
%%
handle_info({tcp_closed, _Socket}, State) ->
%%    wf:info(?MODULE, "TCP connection CLOSED with state: ~p~n", [State]),
    {noreply, set_last_active_time(State#state{socket = undefined})};

%%
%%  Receive msg about connection had errors
%%
handle_info({tcp_error, _Socket, Reason}, State) ->
    wf:info(?MODULE, "TCP connection got ERROR: ~p with state: ~p~n", [Reason, State]),
    {noreply, set_last_active_time(State#state{socket = undefined})};
handle_info(hibernate, State) ->
    case sgi:time_now() > (State#state.last_active_time + ?HIBERNATE_AFTER) of
        true ->
            State1 = close(State),
            {noreply, State1, hibernate};
        _ ->
            {noreply, State}
    end;

%%
%% Close connection
%%
handle_info(close, State) ->
    State1 = close(State),
    {noreply, set_last_active_time(State1)};
handle_info(send_alive, State) -> % @todo it obtains overload if more than 1000 processes will send this message
    case sgi:is_alive(sgi_arbiter) of
        true -> sgi_arbiter:new_pool_started(self());
        _ -> wait
    end,
    {noreply, State};
handle_info(Info, State) ->
    wf:error(?MODULE, "Unexpected message: ~p~n", [Info]),
    {noreply, set_last_active_time(State)}.

terminate(_Reason, State) ->
    close(State),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(State) -> connect(State, once).

%%
%% Process is trying to connect to a server.
%%
-spec connect(#state{}, tcp_active_type()) ->
    {ok, State :: #state{}} | {error, Reason :: term(), State :: #state{}}.

connect(State = #state{socket = undefined, address = Address, port = Port}, Active) ->

    {Address1, Port1} =
    case wf:config(sgi, proxy) of
        #{address := A, port := P} -> {A, P};
        _ -> {Address, Port}
    end,

    case gen_tcp:connect(Address1, Port1, [binary], State#state.timeout) of
        {ok, Socket} ->
            ProxyResult =
            case wf:config(sgi, proxy) of
                #{type := socks5} ->
                    sgi_socks5:connect(Socket, Address, Port);
                %% @todo Add other proxy
                _ ->
                    ok
            end,
            case ProxyResult of
                ok ->
                    State1 = success_connect(Socket, State, Active),
                    {ok, State1};
                Error ->
                    wf:error(?MODULE, "Proxy connect error: ~p~n", [Error]),
                    gen_tcp:close(Socket),
                    State1 = fail_connect(State),
                    {error, Error, State1}
            end;
        {error, Reason} ->
            State2 = fail_connect(State),
            {error, Reason, State2}
    end;
connect(#state{socket = Socket} = State, false) ->
%%connect(State, false) ->
    inet:setopts(Socket, [{active, false}]),
%%    State1 = close(State),
%%    connect(State1, false);
    {ok, State};
connect(State = #state{socket = Socket}, Active) ->
    case erlang:port_info(Socket, id) of
        undefined ->
            State1 = State#state{socket = undefined},
            connect(State1, Active);
        _ ->
            {ok, State}
    end.

success_connect(Socket, State, Active) ->
    inet:setopts(Socket, [{active, Active}]),
    State#state{socket = Socket, fails = 0}.

fail_connect(State) ->
    State1 = State#state{fails = State#state.fails + 1},
    overage_fail_conns(State1).

-spec close(#state{}) -> #state{}.
close(State) when State#state.socket /= undefined ->
    gen_tcp:close(State#state.socket),
    State#state{socket = undefined};
close(State) -> State.

do_recv(State, Bs) ->
    case gen_tcp:recv(State#state.socket, 0) of
        {ok, B} ->
            do_recv(State, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)};
        {error, Reason} ->
            {error, Reason}
    end.

%%
%% Receive only one msg
%%
-spec do_recv_once(#state{}) -> {ok, term()} | {error, term()}.
do_recv_once(State) ->
    case gen_tcp:recv(State#state.socket, 0) of
        {ok, B} ->
            {ok, B};
        {error, Reason} ->
            {error, Reason}
    end.

-spec set_last_active_time(#state{}) -> #state{}.
set_last_active_time(State) ->
    State#state{last_active_time = sgi:time_now()}.

%%
%% Tell arbiter what connection is fail
%%
-spec overage_fail_conns(#state{}) -> #state{}.
overage_fail_conns(State) ->
    case State#state.fails >= State#state.max_fails of
        true ->
            sgi_arbiter:down(self(), State#state.failed_timeout),
            State#state{fails = 0};
        _ ->
            State
    end.

