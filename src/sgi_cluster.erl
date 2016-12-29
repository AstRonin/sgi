-module(sgi_cluster).
-behaviour(gen_server).

%% API
-export([start_link/0, node_info/0, is_ready/0, send/3, is_use/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).


%% #{
%% node@localhost => #{name => '', available => true, overloaded => false, last_updated => 0}
%% node1@localhost => #{name => '', available => true, overloaded => false, last_updated => 0}
%% }
-record(state, {nodes = #{}}).
-record(node_info, {name = '', available = true, overloaded = false, last_updated = 0}).
%%-record(cluster_info, {comps = []}).
%%-record(node_info, {comps = [#{name => '', available => true, overloaded => false, last_updated => 0}]}).

%%%===================================================================
%%% API
%%%===================================================================

is_use() ->
    case wf:config(sgi, cluster, false) of false -> false; _ -> true end.

node_info() ->
    gen_server:call(?SERVER, node_info).

%% send(sgi_n2o_fcgi_handler, do_send, [CGIParams, HasBody, Body])
-spec send(M :: atom(), F :: atom(), A :: []) -> Return :: term().
send(M, F, A) ->
    C = {sgi_monitoring:is_critical(), idle_node()},
    case C of
        {true, Node} ->
            Key = rpc:async_call(Node, M, F, A),
            RT = sgi:mv(response_timeout, wf:config(sgi, cluster, #{}), 600000), % 10 minutes
            case rpc:nb_yield(Key, RT) of
                {value, Val} -> Val;
                timeout -> <<>>
            end;
        _ ->
            erlang:apply(M, F, A)
    end.

is_ready() ->
    case whereis(?SERVER) of
        undefined -> false;
        _ -> true
    end.

idle_node() ->
    gen_server:call(?SERVER, idle_node).

%%is_overload() ->
%%    sgi_monitoring:is_overload().

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    check_node(),
    M = check_nodes(),
    self() ! collect_load_info,
    {ok, #state{nodes = M}}.

%%-------------------------------------------------------------------
%% Handle Call
%%-------------------------------------------------------------------

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(node_info, _From, State) ->
    {reply, State#state.nodes, State};
handle_call(idle_node, _From, State) ->
    R = do_idle_node(State),
    {reply, R, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%-------------------------------------------------------------------
%% Handle Cast
%%-------------------------------------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------
%% Handle Call
%%-------------------------------------------------------------------

handle_info({From, is_overload}, State) ->
    From ! {sgi_cluster, erlang:node(), {ok, node(), sgi_monitoring:is_overload()}},
    {noreply, State};
handle_info(collect_load_info, State) ->
    spawn(fun() ->
        R = collect_load_info(),
        sgi_cluster ! {collect_load_info_result, R} end),
    SI = sgi:mv(syncr_interval, wf:config(sgi, cluster, #{}), 60000),
    timer:send_after(SI, collect_load_info),
    {noreply, State};
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

%% Send request of info to all nodes
collect_load_info() ->
    rpc:multi_server_call(nodes(), sgi_cluster, is_overload).

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
                RecordInfo#node_info{available = true, overloaded = Resp, last_updated = unix_time()};
            NodeName ->
                RecordInfo#node_info{available = false, last_updated = unix_time()}
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

check_nodes() ->
    Nodes = sgi:mv(nodes, wf:config(sgi, cluster, #{}), []),
    [net_kernel:connect_node(N) || N <- Nodes],
    check_nodes(erlang:nodes(), #{}).
check_nodes([], M) ->
    M;
check_nodes([NodeName | T], M) ->
    M1 = maps:put(NodeName, #node_info{name = NodeName}, M),
    check_nodes(T, M1).

-spec do_idle_node(State :: #state{}) -> boolean().
do_idle_node(State) ->
    M = State#state.nodes,
    L = maps:to_list(M),
    do_idle_node1(L).

do_idle_node1([]) ->
    node();
do_idle_node1([H|T]) ->
    case node_is_idle(H) of
        true ->
            true;
        _ ->
            do_idle_node1(T)
    end.

node_is_idle(R) ->
    case R of
        R when R#node_info.available, R#node_info.overloaded =:= false ->
            true;
        _ ->
            false
    end.

unix_time() ->
    erlang:system_time(seconds).