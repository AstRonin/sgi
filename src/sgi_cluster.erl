-module(sgi_cluster).
-behaviour(gen_server).

%% API
-export([start_link/0, node_info/0, is_ready/0, send/3, is_use/0, is_overloaded/0, info/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

%% Sample of structure of State
%% #{
%% node@localhost => #{name => '', available => true, overloaded => false, last_updated => 0}
%% node1@localhost => #{name => '', available => true, overloaded => false, last_updated => 0}
%% }
-record(node_info, {name = '', available = true, overloaded = false, updated = 0}).
-record(state, {nodes = #{}, last_sent_node = 0, self = #node_info{}}).

%%%===================================================================
%%% API
%%%===================================================================

is_use() ->
    case wf:config(sgi, cluster, false) of false -> false; _ -> true end.

node_info() ->
    gen_server:call(?SERVER, node_info).

info() ->
    gen_server:call(?SERVER, info).

-spec send(M :: atom(), F :: atom(), A :: []) -> Return :: term().
send(M, F, A) ->
    case is_overloaded() of
        true ->
            wf:info(?MODULE, "Try call to cluster", []),
            Key = rpc:async_call(idle_node(), M, F, A),
            case rpc:nb_yield(Key, rt()) of
                {value, Val} -> Val;
                timeout -> <<>>
            end;
        _ ->
            wf:info(?MODULE, "Have no availeble nodes, calling self node", []),
            erlang:apply(M, F, A)
    end.

is_ready() ->
    case whereis(?SERVER) of
        undefined -> false;
        _ -> true
    end.

is_overloaded() ->
    gen_server:call(?SERVER, is_overloaded).

idle_node() ->
    gen_server:call(?SERVER, idle_node).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    check_node(),
    M = check_nodes(),
    self() ! collect_load_info,
    {ok, #state{nodes = M, self = #node_info{name = node()}}}.

%%-------------------------------------------------------------------
%% Handle Call
%%-------------------------------------------------------------------

handle_call(node_info, _From, State) ->
    {reply, State#state.nodes, State};

handle_call(info, _From, State) ->
    {reply, State, State};

handle_call(is_overloaded, _From, State) ->
    {reply, State#state.self#node_info.overloaded, State};

handle_call(idle_node, _From, State) ->
    {ok, NodeName, Index} = do_idle_node(State),
    wf:info(?MODULE, "Selected node for a help: ~p~n", [NodeName]),
    State1 = State#state{last_sent_node = Index},
    {reply, NodeName, State1};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%-------------------------------------------------------------------
%% Handle Cast
%%-------------------------------------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------
%% Handle Info
%%-------------------------------------------------------------------

%% @doc Request to other nodes.
handle_info({From, is_overload}, State) ->
    From ! {sgi_cluster, erlang:node(), {ok, node(), sgi_monitoring:is_overload()}},
    {noreply, State};

%% @doc Self request for collect information. To do each minute.
handle_info(collect_load_info, State) ->
    SI = sgi:mv(syncr_interval, wf:config(sgi, cluster, #{}), 60000),
    spawn(fun() ->
        {ok, TRef} = timer:kill_after(SI - 5000), % kill current process because will soon open new the same.
        wf:info(?MODULE, "Send is_overload request to cluster", []),
        R = rpc:multi_server_call(nodes(), sgi_cluster, is_overload),
        wf:info(?MODULE, "Recieve claster info: ~p~n", [R]),
        timer:cancel(TRef),
        sgi_cluster ! {collect_load_info_result, R}
          end),

    Self = State#state.self,
    State1 = State#state{self = Self#node_info{overloaded = sgi_monitoring:is_overload(), updated = unix_time()}},

    timer:send_after(SI, collect_load_info),
    {noreply, State1};

%% @doc Parse result from other nodes.
handle_info({collect_load_info_result, R}, State) ->
    State1 = cluster_info_response(R, State),
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec cluster_info_response(Response, State) -> State1 when
    Response :: {Replies, BadNodes} | Replies | BadNodes | [],
        Replies :: [{ok, NodeName, Resp}], %% Responses from each node
        BadNodes :: [NodeName],
            NodeName :: atom(), %% Name of remote node
            Resp :: term(), %% Response from remote node
    State :: #state{},
    State1 :: #state{}.
cluster_info_response([], State) ->
    State;
cluster_info_response({[], []}, State) ->
    State#state{nodes = #{}};
cluster_info_response({Replies, BadNodes}, State) ->
    State1 = cluster_info_response(Replies, State),
    State2 = cluster_info_response(BadNodes, State1),
    State2;
cluster_info_response([H | T], State) ->
    NodeName = response_node_name(H),
    {RecordInfo, M} =
        case map_size(State#state.nodes) > 0 of
            true ->
                #{NodeName := RecordInfo1} = M1 = State#state.nodes,
                {RecordInfo1, M1};
            false ->
                {#node_info{name = NodeName}, #{}}
        end,
    NewRecord =
        case H of
            {ok, NodeName, Resp} ->
                RecordInfo#node_info{available = true, overloaded = Resp, updated = unix_time()};
            NodeName ->
                RecordInfo#node_info{available = false, updated = unix_time()}
        end,
    M2 = M#{NodeName => NewRecord},
    State1 = State#state{nodes = M2},
    cluster_info_response(T, State1).

response_node_name({ok, N, _}) -> N;
response_node_name(N) -> N.

%% Check node and change if needed. Don't forget run `epmd -daemon`
check_node() ->
    case erlang:node() =:= nonode@nohost of
        true ->
            net_kernel:start(['node@127.0.0.1']);
        _ ->
            ok
    end.

-spec check_nodes() -> Map when
    Map :: #{node() => #node_info{}} | #{}.
check_nodes() ->
    Nodes = sgi:mv(nodes, wf:config(sgi, cluster, #{}), []),
    [net_kernel:connect_node(N) || N <- Nodes],
    check_nodes(erlang:nodes(), #{}).
check_nodes([], M) ->
    M;
check_nodes([NodeName | T], M) ->
    M1 = maps:put(NodeName, #node_info{name = NodeName}, M),
    check_nodes(T, M1).

-spec do_idle_node(State :: #state{}) -> {ok, NodeName, Index} when
    NodeName :: list(),
    Index :: non_neg_integer().
do_idle_node(State) ->
    L = maps:values(State#state.nodes),
    I = State#state.last_sent_node,
    do_idle_node(L, I + 1, I).

-spec do_idle_node(L, I, SelfI) -> term() when
    L :: list(), % List of nodes
    I :: non_neg_integer(), % Iterationable index for searching node
    SelfI :: non_neg_integer(). % Current index for avoid cycle and search only in one circle
do_idle_node([], _I, _) ->
    {ok, node(), 0};
%%do_idle_node([{NodeInfo}], _I, _SelfI) -> %% using for performance trick if we have only one helped node in our cluster
%%    case NodeInfo of
%%        NodeInfo when NodeInfo#node_info.available, not NodeInfo#node_info.overloaded ->
%%            {ok, NodeInfo#node_info.name, 1};
%%        _ ->
%%            {ok, node(), 0}
%%    end;
do_idle_node(L, I, SelfI) ->
    try
        wf:info(?MODULE, "List of nodes: ~p~n", [L]),
        wf:info(?MODULE, "SelfI: ~p~n", [SelfI]),
        case lists:nth(I, L) of
            NodeInfo when NodeInfo#node_info.available, not NodeInfo#node_info.overloaded ->
                {ok, NodeInfo#node_info.name, I};
            _ ->
                case I == SelfI of
                    true ->
                        do_idle_node([], 0, 0);
                    _ ->
                        do_idle_node(L, I + 1, SelfI)
                end
        end
    catch
        error:_Reason ->
            case SelfI of
                0 ->
                    do_idle_node([], 0, 0); % call exit function because nothing found
                _ ->
                    do_idle_node(L, 1, SelfI) % 1 - is first element in array
            end
    end.

rt() ->
    sgi:mv(response_timeout, wf:config(sgi, cluster, #{}), 600000). % 10 minutes

unix_time() ->
    erlang:system_time(seconds).