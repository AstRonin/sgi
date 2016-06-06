%%
%% @todo Add check active Pool Pocesses (sgi_pool), run ch_to_state each 10(for example) minutes
%%

-module(sgi_arbiter).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, ch_to_state/0, alloc/0, free/1, free_all/0, list/0]).

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

-define(PROC_LIST, proc_list).
-define(PROC_ORDER_NUM, proc_order_num).

-record(state, {}).
-record(proc, {pid, status = ?AVAILABLE, time}).

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
    alloc(20).
alloc(0) ->
    {ok, undefined};
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


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    wf:info(?MODULE, "Init: ~n", []),
    {ok, #state{}};
init([Ch]) ->
    wf:info(?MODULE, "Init with children: ~p~n", [Ch]),
    ch_to_state(Ch, []),
    {ok, #state{}}.


handle_call(ch_to_state, _From, State) ->
    Ch = supervisor:which_children(?SUPERVISOR),
    wf:info(?MODULE, "Children: ~p~n", [Ch]),
    ch_to_state(Ch, []),
    {reply, ok, State};
handle_call(alloc, _From, State) ->
    {reply, {ok, active()}, State};
handle_call(list, _From, State) ->
    {reply, {ok, get_list()}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({free, Pid}, State) ->
    free(Pid, wf:state(?PROC_LIST), []),
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

%%ch_to_state([Ch|Children]) ->
%%    case erlang:hd(erlang:element(4, Ch)) of
%%        sgi_pool -> % @todo don't forget about this module name.
%%            wf:state(erlang:element(2, Ch), #proc{pid = erlang:element(2, Ch), time = erlang:system_time(seconds)});
%%        _ -> skip
%%    end,
%%    ch_to_state(Children);
%%ch_to_state([]) ->
%%    ok.


%%ch_to_state(Ch) ->
%%    ch_to_state(Ch, []).
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

get_list() ->
    get(?PROC_LIST).

active() ->
    L = get(?PROC_LIST),
    active(L, [], undefined).
active([H|T], L, Ret) ->
    case H#proc.status of
        ?AVAILABLE when Ret == undefined ->
            R = H#proc{status = ?NOTAVAILABLE},
            active(T, L ++ [R], R#proc.pid);
        _ ->
            active(T, L ++ [H], Ret)
    end;
active([], _, undefined) ->
    undefined;
active([], L, Pid) ->
    wf:state(?PROC_LIST, L),
    Pid.
%%    active_nearly().

%%free(Pid) ->
%%    L = wf:state(?PROC_LIST),
%%    free(Pid, L, []).
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

free_all([H|T], L) ->
    R = H#proc{status = ?AVAILABLE},
    free_all(T, L ++ [R]);
free_all([], L) ->
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

