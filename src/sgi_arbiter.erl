%%
%% @todo Add check active Pool Pocesses (sgi_pool), run ch_to_state each 10(for example) minutes
%%
-module(sgi_arbiter).

-behaviour(gen_server).

%% API
-export([start_link/0,
    start_link/1,
    alloc/0,
    free/1,
    new_pool_started/1,
    free_all/0,
    list/0,
    map/0,
    down/2]).

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
-define(PROC_MAP, proc_map).
-define(PROC_BY_SERVER, proc_by_server).

-define(PROC_ORDER_NUM, proc_order_num).

-record(state, {adding_new_pool = 0, ch_to_state_timer}).
-record(proc, {pid, weight = 1, server_name, status = ?AVAILABLE, time_created = 0, failed_out = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start_link(A) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [A], []).

alloc() ->
    alloc(300).
alloc(0) ->
    {error, "Attempts have ended"};
alloc(CountTry) ->
    case gen_server:call(?SERVER, alloc, 600000) of % 10 minutes
        {ok, undefined} ->
            timer:sleep(100), % @todo add changing this number dynamically
            alloc(CountTry - 1);
        {ok, Pid} -> {ok, Pid}
    end.

free(Pid) ->
    gen_server:cast(?SERVER, {free, Pid}).

new_pool_started(Pid) ->
    gen_server:cast(?SERVER, {new_pool_started, Pid}).

free_all() ->
    gen_server:cast(?SERVER, free_all).

list() ->
    gen_server:call(?SERVER, list).
map() ->
    gen_server:call(?SERVER, map).

down(Pid, FailedTimeout) ->
    gen_server:cast(?SERVER, {down, Pid, FailedTimeout}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    self() ! ch_to_state,
    {ok, #state{adding_new_pool = 1}}.


handle_call(alloc, _From, State) ->
    V = case active() of
        undefined ->
            self() ! add_pool,
            undefined;
        Pid -> Pid
    end,
    {reply, {ok, V}, State};
handle_call(list, _From, State) ->
    {reply, get_list(), State};
handle_call(map, _From, State) ->
    {reply, get_map(), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({down, Pid, FailedTimeout}, State) ->
    down1(Pid, FailedTimeout),
    {noreply, State};
handle_cast({free, Pid}, State) ->
    free1(Pid),
    {noreply, State};
handle_cast({new_pool_started, Pid}, State) ->
    new_pool_started(Pid, State),
    {noreply, State};
handle_cast(free_all, State) ->
    free_all1(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(add_pool, #state{adding_new_pool = 1} = State) ->
    {noreply, State};
handle_info(add_pool, State) ->
    State1 = case add_pool() of
        N when is_integer(N) andalso N > 0 ->
            T = erlang:send_after(100, self(), ch_to_state),
            State#state{adding_new_pool = 1, ch_to_state_timer = T};
        _ ->
            State
    end,
    {noreply, State1};
handle_info(ch_to_state, State) ->
    sgi:ct(State#state.ch_to_state_timer),
    Ch = supervisor:which_children(?SUPERVISOR),
    ch_to_state(Ch),
    {noreply, State#state{adding_new_pool = 0, ch_to_state_timer = undefined}};
handle_info(Info, State) ->
    wf:error(?MODULE, "Unknown Request: ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_pool() ->
    S = wf:state(?PROC_BY_SERVER),
    add_pool(maps:values(S), 0).
add_pool([H|T], AddedNum) ->
    Sett = maps:get(settings, H, #{}),
    MaxConnCount = maps:get(max_connections, Sett, 1),
    ProcCount = length(maps:get(processes, H, [])),
    AddedNum1 = AddedNum + case ProcCount < MaxConnCount of
        true ->
            Num = increase_pool_count(ProcCount, MaxConnCount),
            spawn(fun() -> sgi_sup:start_pool_children(Num, maps:to_list(Sett)) end),
            Num;
        _ ->
            0
    end,
    add_pool(T, AddedNum1);
add_pool([], AddedNum) -> AddedNum.

increase_pool_count(Num, Max) -> % Num * 2 * ratio, ratio - 0.8 and can be changing
    Num1 = round(Num * 2 * 0.8),
    case Num1 > Max of true -> Max - Num; _ -> Num1 - Num end.

ch_to_state(Ch) ->
    ch_to_state(lists:reverse(Ch), []).
ch_to_state([Ch|Children], NewList) ->
    case hd(element(4, Ch)) of
        sgi_pool -> % @todo fon't forget about this module name if will be changes of name of module.
            ch_to_state(Children, [make_pool(element(2, Ch))|NewList]);
        _ ->
            ch_to_state(Children, NewList)
    end;
ch_to_state([], NewList) ->
    proc_sort(NewList),
    ok.

make_pool(Pid) ->
    {ok, M} = sgi_pool:settings(Pid),
    #proc{
        pid = Pid,
        time_created = sgi:time_now(),
        weight = maps:get(weight, M),
        server_name = maps:get(server_name, M)}.

proc_sort(Processes) ->
    MapByServer = group_by_server(Processes),
    New1 = case wf:config(sgi, balancing_method) of
        blurred ->
            blurred(MapByServer, length(Processes));
        _ ->
            Fun = fun(A,B) -> A#proc.weight > B#proc.weight end,
            SortedList = lists:sort(Fun, Processes),
            lists:map(fun(P) -> P#proc.pid end, SortedList)
    end,
    Temp = lists:map(fun(P) -> {P#proc.pid,P} end, Processes),
    Map = maps:from_list(Temp),
    OldMap = case wf:state(?PROC_MAP) of undefined -> #{}; M1 -> M1 end,
    wf:state(?PROC_MAP, maps:merge(Map, OldMap)),
    wf:state(?PROC_BY_SERVER, MapByServer),
    wf:state(?PROC_LIST, New1).

group_by_server(Ch) ->
    S = wf:config(sgi, servers),
    group_by_server(S, Ch, 1, #{}).
group_by_server([H|T], Ch, N, M) ->
    Settings = maps:from_list(H),
    ServerName = maps:get(name, Settings),
    M1 = M#{ServerName => #{settings => Settings, processes => ch_by_server(Ch, ServerName)}},
    group_by_server(T, Ch, N + 1, M1);
group_by_server([], _, _, M) ->
    M.

ch_by_server(L, Name) ->
    ch_by_server(L, Name, []).
ch_by_server([H|T], Name, Acc) ->
    case H#proc.server_name == Name of
        true -> ch_by_server(T, Name, [H#proc.pid|Acc]);
        _ -> ch_by_server(T, Name, Acc)
    end;
ch_by_server([], _, Acc) ->
    Acc.

blurred(M, Total) ->
    blurred(M, Total, []).
blurred(M, Total, Acc) ->
    ServerNames = maps:keys(M),
    blurred(ServerNames, M, Total, Acc).
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
    wf:state(?PROC_LIST).
get_map() ->
    wf:state(?PROC_MAP).

-spec active() -> pid() | undefined.
active() ->
    Map = wf:state(?PROC_MAP),
    List = wf:state(?PROC_LIST),
    Self= self(),
    Ref = make_ref(),
    spawn(fun() -> R = active(List, Map), Self ! {R,Ref} end),
    Ret1 = receive {Ret,Ref} -> Ret end,

    case Ret1 of
        undefined ->
            undefined;
        P ->
            wf:state(?PROC_MAP, Map#{P#proc.pid := P#proc{status = ?NOTAVAILABLE}}),
            P#proc.pid
    end.

-spec active(list(), map()) -> #proc{} | undefined.
active([H|T], M) ->
    P = maps:get(H,M,#proc{}),
    case P#proc.status of
        ?AVAILABLE ->
            availabled(P,T,M);
        ?DOWN ->
            downed(P,T,M);
        _ -> active(T,M)
    end;
active([], _) -> undefined.

availabled(P,T,M) ->
    case check_alive(P) andalso check_connect(P) of true->P;_->active(T,M) end.
downed(P,T,M) ->
    case check_pass_failed_time(P) andalso check_alive(P) andalso check_connect(P) of true->P;_->active(T,M) end.

check_connect(#proc{pid = Pid}) ->
    case sgi_pool:try_connect(Pid) of ok->true;error->false end.

check_alive(#proc{pid = Pid}) ->
    sgi:is_alive(Pid).

check_pass_failed_time(P)->
    sgi:time_now() > P#proc.failed_out.

free1(Pid) ->
    Map = wf:state(?PROC_MAP),
    case maps:find(Pid, Map) of
        {ok, P} -> wf:state(?PROC_MAP, Map#{Pid := P#proc{status = ?AVAILABLE}});
        _ -> ok
    end.

free_all1() ->
    Map = wf:state(?PROC_MAP),
    Fun = fun(_K,V1)->case V1#proc.status==?NOTAVAILABLE of true->V1#proc{status=?AVAILABLE}; _->V1 end end,
    Map1 = maps:map(Fun, Map),
    wf:state(?PROC_MAP, Map1).

down1(Pid, FailedTimeout) ->
    Map = wf:state(?PROC_MAP),
    case maps:find(Pid, Map) of
        {ok, P} -> wf:state(?PROC_MAP, Map#{Pid:=P#proc{status=?DOWN,failed_out=(sgi:time_now()+FailedTimeout)}});
        _ -> ok
    end.



new_pool_started(Pid, _State) -> % when sgi:is_alive(Pid) ->
    Pool = make_pool(Pid),

    Map = wf:state(?PROC_MAP),

    ServerName = maps:get(Pool#proc.server_name, Map, #{}),
    PoolList = maps:get(processes, ServerName, []),

    wf:state(?PROC_MAP, Map#{Pool#proc.server_name => ServerName#{processes => PoolList ++ [Pool]}}),
    wf:state(?PROC_LIST, wf:state(?PROC_LIST) ++ [Pool]).
