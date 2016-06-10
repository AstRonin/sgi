%%
%% @todo Add check active Pool Pocesses (sgi_pool), run ch_to_state each 10(for example) minutes
%%

-module(sgi_arbiter).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, ch_to_state/0, alloc/0, free/1, free_all/0, list/0, down/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(SUPERVISOR, sgi_sup).

-define(AVAILABLE, 1).
-define(NOTAVAILABLE, 2).
-define(DOWN, 3).

-define(PROC_LIST, proc_list).
-define(PROC_ORDER_NUM, proc_order_num).

-record(state, {}).
-record(proc, {pid, weight = 1, server_name, status = ?AVAILABLE, time_created = 0, failed_out = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start_link(A) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [A], []).

ch_to_state() ->
    gen_server:call(?SERVER, ch_to_state).

alloc() ->
    alloc(10).
alloc(0) ->
    {error, "Attempts have ended"};
alloc(CountTry) ->
    case gen_server:call(?SERVER, alloc) of
        {ok, undefined} ->
            wf:error(?MODULE, "Pid is undefined, waiting 1000 ms ~n", []),
            timer:sleep(1000),
            alloc(CountTry - 1);
        {ok, Pid} -> {ok, Pid}
    end.

free(Pid) ->
    gen_server:cast(?SERVER, {free, Pid}).

free_all() ->
    gen_server:cast(?SERVER, free_all).

list() ->
    gen_server:call(?SERVER, list).

down(Pid, FailedTimeout) ->
    gen_server:cast(?SERVER, {down, Pid, FailedTimeout}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}};
init([Ch]) ->
    wf:info(?MODULE, "Init with children: ~p~n", [Ch]),
    ch_to_state(Ch),
    wf:info(?MODULE, "Processes: ~p~n", [wf:state(?PROC_LIST)]),
    {ok, #state{}}.


handle_call(ch_to_state, _From, State) ->
    Ch = supervisor:which_children(?SUPERVISOR),
    ch_to_state(Ch, []),
    {reply, ok, State};
handle_call(alloc, _From, State) ->
    {reply, {ok, active()}, State};
handle_call(list, _From, State) ->
    {reply, {ok, get_list()}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({down, Pid, FailedTimeout}, State) ->
    wf:info(?MODULE, "Pool worker is down: ~p~n", [Pid]),
    down({Pid, FailedTimeout}),
    {noreply, State};
handle_cast({free, Pid}, State) ->
    free(Pid, wf:state(?PROC_LIST)),
    {noreply, State};
handle_cast(free_all, State) ->
    free_all(wf:state(?PROC_LIST), []),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    wf:info(?MODULE, "Unknown Request: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ch_to_state(Ch) ->
    ch_to_state(lists:reverse(Ch), []).
ch_to_state([Ch|Children], NewList) ->
    case erlang:hd(erlang:element(4, Ch)) of
        sgi_pool -> % @todo fon't forget about this module name if will be changes of name of module.
            Pid = erlang:element(2, Ch),
            {ok, M} = sgi_pool:settings(Pid),
            New = [#proc{
                pid = Pid,
                time_created = time_now(),
                weight = maps:get(weight, M),
                server_name = maps:get(server_name, M)}] ++ NewList,
            ch_to_state(Children, New);
        _ ->
            ch_to_state(Children, NewList)
    end;
ch_to_state([], New) ->
    proc_sort(New),
    ok.

proc_sort(Processes) ->
    MapByServer = group_by_server(Processes), % @todo Will need save the Map of Servers when we will be to use
%%    wf:info(?MODULE, "MapByServer: ~p~n", [MapByServer]),
    New1 = case wf:config(sgi, balancing_method) of
        blurred -> blurred(MapByServer, length(Processes));
        _ -> lists:sort(fun(A,B) -> A#proc.weight > B#proc.weight end, Processes)
    end,
    wf:state(?PROC_LIST, New1).

group_by_server(Ch) ->
    S = wf:config(sgi, servers),
    group_by_server(S, Ch, 1, #{}).
group_by_server([H|T], Ch, N, M) ->
    K = "Server#" ++ wf:to_list(N),
    Settings = maps:from_list(H),
    M1 = M#{K => #{settings => Settings, processes => ch_by_server(Ch, maps:get(name, Settings))}},
    group_by_server(T, Ch, N + 1, M1);
group_by_server([], _, _, M) ->
    M.

ch_by_server(L, Name) ->
    ch_by_server(L, Name, []).
ch_by_server([H|T], Name, Acc) ->
    case H#proc.server_name == Name of
        true -> ch_by_server(T, Name, [H|Acc]);
        _ -> ch_by_server(T, Name, Acc)
    end;
ch_by_server([], _, Acc) ->
    Acc.

blurred(M, Total) ->
    blurred(M, Total, []).
blurred(M, Total, Acc) ->
    Keys = maps:keys(M),
    blurred(Keys, M, Total, Acc).
blurred([H|T], M, Total, Acc) ->
    case maps:get(processes, maps:get(H, M, []), []) of
        [] ->
            blurred(T, M, Total, Acc);
        [H1|T1] ->
            M1 = M#{H => #{processes => T1}},
            blurred(T, M1, Total, [H1|Acc])
    end;
blurred([], M, Total, Acc) ->
    case Total == length(Acc) of
        true -> lists:reverse(Acc);
        _ -> blurred(M, Total, Acc)
    end.

get_list() ->
    get(?PROC_LIST).



active() ->
    L = get(?PROC_LIST),
    active(L, [], undefined).
active([#proc{status = ?AVAILABLE} = H | T], L, undefined) ->
    check_connect_and_activate(H, T, L);
active([#proc{status = ?DOWN} = H | T], L, undefined) ->
    case check_pass_failed_time(H) of
        true -> check_connect_and_activate(H, T, L);
        _ -> active(T, [H|L], undefined)
    end;
active([H|T], L, Ret) ->
    active(T, [H|L], Ret);
active([], _, undefined) ->
    undefined;
active([], L, Pid) ->
    wf:state(?PROC_LIST, lists:reverse(L)),
    Pid.
%%    active_nearly().

check_connect_and_activate(H, T, L) ->
    case sgi_pool:try_connect(H#proc.pid) of
        ok -> check_process_alive_before_activate(H, T, L);
        error -> active(T, [H|L], undefined)
    end.

check_process_alive_before_activate(#proc{pid = Pid} = H, T, L) ->
    case erlang:is_process_alive(Pid) of
        true -> active(T, [H#proc{status = ?NOTAVAILABLE}|L], Pid);
        _ -> active(T, L, undefined) % @todo Remove bad Process from Map of Servers
    end.

check_pass_failed_time(H)->
    time_now() > H#proc.failed_out.

free(Pid, L) ->
    free(Pid, L, []).
free(undefined, _, _) ->
    ok;
free(Pid, [H|T], L) ->
    case Pid == H#proc.pid of
        true ->
            R = H#proc{status = ?AVAILABLE},
            free(Pid, T, [R|L]);
        _ ->
            free(Pid, T, [H|L])
    end;
free(_, [], L) ->
    wf:state(?PROC_LIST, lists:reverse(L)).

free_all([#proc{status = ?DOWN} = H|T], L) ->
    free_all(T, [H|L]);
free_all([H|T], L) ->
    R = H#proc{status = ?AVAILABLE},
    free_all(T, [R|L]);
free_all([], L) ->
    wf:state(?PROC_LIST, lists:reverse(L)).

down(Data) ->
    L = get(?PROC_LIST),
    down(Data, L, []).
down({Pid, FailedTimeout} = Data, [H|T], L) ->
    case Pid == H#proc.pid of
        true ->
            R = H#proc{status = ?DOWN, failed_out = (time_now() + FailedTimeout)},
            down(Data, T, [R|L]);
        _ ->
            down(Data, T, [H|L])
    end;
down(_, [], L) ->
    wf:state(?PROC_LIST, lists:reverse(L)),
    ok.


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

time_now() ->
    erlang:system_time(seconds).