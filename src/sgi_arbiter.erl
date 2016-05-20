-module(sgi_arbiter).

-behaviour(gen_server).

%% API
-export([start_link/0, alloc/0, free/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(AVAILABLE, 1).
-define(NOTAVAILABLE, 2).

-define(PROC_LIST, proc_list).
-define(PROC_ORDER_NUM, proc_order_num).

-record(state, {}).
-record(proc, {pid, status = ?AVAILABLE, time}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

alloc() ->
    gen_server:call(?SERVER, alloc).

free(Pid) ->
    gen_server:cast(?SERVER, {free, Pid}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Parent]) ->

    Children = supervisor:which_children(Parent),
    ch_to_state(Children),
    {ok, #state{}}.


handle_call(alloc, _From, State) ->
    {reply, {ok, active()}, State};
%%handle_call({set_free, Pid}, _From, State) ->
%%    {reply, {ok, free(Pid)}, State}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({free, Pid}, State) ->
    free(Pid),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%ch_to_state([Ch|Children]) ->
%%    case erlang:hd(erlang:element(4, Ch)) of
%%        sgi_pool -> % @todo fon't forget about this module name.
%%            wf:state(erlang:element(2, Ch), #proc{pid = erlang:element(2, Ch), time = erlang:system_time(seconds)});
%%        _ -> skip
%%    end,
%%    ch_to_state(Children);
%%ch_to_state([]) ->
%%    ok.

ch_to_state(Ch) ->
    ch_to_state(Ch, []).
ch_to_state([Ch|Children], NewList) ->
    case erlang:hd(erlang:element(4, Ch)) of
        sgi_pool -> % @todo fon't forget about this module name if will be changes of name of module.
            New = [#proc{pid = erlang:element(2, Ch), time = erlang:system_time(seconds)}] ++ NewList,
            ch_to_state(Children, New);
%%            wf:state(erlang:element(2, Ch), #proc{pid = erlang:element(2, Ch), time = erlang:system_time(seconds)});
        _ -> ch_to_state(Children, NewList)
    end;
ch_to_state([], New) ->
    wf:state(?PROC_LIST, New),
    ok.

active() ->
    L = get(?PROC_LIST),
    active(L, [], undefined).
active([H|T], L, Ret) ->
    case H#proc.status of
        ?AVAILABLE ->
            R = H#proc{status = ?NOTAVAILABLE},
            active(T, [R] ++ L, R#proc.pid);
        _ ->
            active(T, [H] ++ L, Ret)
    end;
active([], _, undefined) ->
    active();
active([], L, P) ->
    wf:state(?PROC_LIST, L),
    P.
%%    active_nearly().

free(Pid) ->
    L = wf:state(?PROC_LIST),
    free(Pid, L, []).
free(Pid, [H|T], L) ->
    case Pid == H#proc.pid of
        true ->
            R = H#proc{status = ?AVAILABLE},
            free(Pid, T, [R] ++ L);
        _ ->
            free(Pid, T, [H] ++ L)
    end;
free(_, [], L) ->
    wf:state(?PROC_LIST, L).

%%active_nearly() ->
%%    List = get(?PROC_LIST),
%%    Order = case get(?PROC_ORDER_NUM) of undefined -> 0; V -> V end,
%%    active_nearly(Order + 1, List).
%%active_nearly(Order, List) when Order > erlang:length(List) ->
%%    active_nearly(1, List);
%%active_nearly(Order, List) ->
%%    I = lists:nth(Order, List),
%%    put(?PROC_ORDER_NUM, Order),
%%    I#proc.pid.

%%active() ->
%%    List = get(?PROC_LIST),
%%    active(List, []).
%%active([H|T], List) ->
%%    case H#proc.status of
%%        ?AVAILABLE -> H#proc.pid;
%%        _ -> active(T)
%%    end;
%%active([], _) ->
%%    active().
%%%%    active_nearly().
%%

