-module(sgi_multiplexer).
-behaviour(gen_server).

-export([start_link/0, start_link/1, set_callback/1]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {req_id_callback}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start_link(A) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [A], []).
set_callback({M,F}) ->
    gen_server:call(?SERVER, {{req_id_callback, M, F}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{M,F}]) ->
    {ok, #state{req_id_callback = {M,F}}}.

handle_call({req_id_callback, M, F}, _From, State) ->
    State1 = State#state{req_id_callback = {M,F}},
    {reply, ok, State1};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({send, Request, PoolPid}, State) ->
    case is_pid(PoolPid) of true -> PoolPid ! {send, Request, self()}; _ -> ok end,
    {noreply, State};
handle_info({send, Request, PoolPid, M, F}, State) ->
    State1 = State#state{req_id_callback = {M,F}},
    case is_pid(PoolPid) of true -> PoolPid ! {send, Request, self()}; _ -> ok end,
    {noreply, State1};
handle_info({socket_return, Data}, State) ->
    {M,F} = State#state.req_id_callback,
    Pid = M:F(Data),
    case is_pid(Pid) of true -> Pid ! {socket_return, Data}; _ -> ok end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

