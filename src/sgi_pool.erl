-module(sgi_pool).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, once/2]).

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
    gen_server:start_link(?MODULE, [], []).
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).
%%start_link() ->
%%    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%start_link(Name) ->
%%    gen_server:start_link({local, ?SERVER}, ?MODULE, [Name], []).

once(Pid, Request) ->
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
            State2 = close_connetion(State1),
            {reply, {ok, B}, State2};
        {error, Reason} ->
            wf:error(?MODULE, "Can't create Socket: ~p~n", [Reason]),
            {reply, {error, Reason}, State}
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
    wf:info(?MODULE, "{tcp, Socket, Data}: ~p~n", [Data]),
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
    wf:error(?MODULE, "Unexpected message: ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    close_connetion(State),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(State) -> connect(State, once).
-spec connect(State, Active :: true | false | once) -> {ok, State} | {error, Reason :: term()}.
connect(State = #state{socket = undefined, address = Address, port = Port}, Active) ->
    case gen_tcp:connect(Address, Port, [binary, {active, Active}]) of
        {ok, Socket} ->
            State1 = State#state{socket = Socket},
            {ok, State1};
        {error, Reason} ->
            {error, Reason}
    end;
connect(State = #state{socket = Socket}, _Active) ->
    case inet:getstat(Socket) of
        {ok, _} -> {ok, State};
        {error, _} ->
            State1 = State#state{socket = undefined},
            connect(State1)
    end.

close_connetion(State) when State#state.socket /= undefined ->
    gen_tcp:close(State#state.socket),
    State#state{socket = undefined}.

do_recv_once(State) ->
    case gen_tcp:recv(State#state.socket, 0) of
        {ok, B} ->
            {ok, B};
        {error, Reason} ->
            wf:error(?MODULE, "do_recv_once: ~p~n", {Reason}),
            {error, Reason}
    end.