-module(sgi_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1, start_child/2, stop_child/1, start_children/0, start_pool_children/2, start_pool_children/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_pool_children(Num, Conf) -> ok when
    Num :: non_neg_integer(),
    Conf :: [tuple()]. % list from sys.config
start_pool_children(Num, Conf) when is_integer(Num) andalso Num > 0 ->
    ChildSpecs = make_pool_spec(Num, Conf, []),
    [supervisor:start_child(?SERVER, ChildSpec) || ChildSpec <- ChildSpecs];
start_pool_children(_,_) -> ok.

-spec start_pool_children() -> ok.
start_pool_children() ->
    ChildSpecs = make_pool_spec(),
    [supervisor:start_child(?SERVER, ChildSpec) || ChildSpec <- ChildSpecs],
    ok.

start_child(N, A) ->
    supervisor:start_child(?MODULE, #{id => N, start => {N, start_link, [A]}}).
start_child(N) ->
    supervisor:start_child(?MODULE, #{id => N, start => {N, start_link, []}}).
stop_child(Name) ->
    case supervisor:terminate_child(?MODULE, Name) of
        ok -> supervisor:delete_child(?MODULE, Name);
        Error -> Error
    end.

start_children() ->
    ok = start_pool_children(),
    ?SERVER:start_child(sgi_arbiter),
    case sgi_cluster:is_use() of
        false -> skip;
        _ -> ?SERVER:start_child(sgi_cluster)
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    spawn(fun() -> start_children() end),
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%
%% Make Child Spec as process (include tcp connections) for starting with supervisor
%%
-spec make_pool_spec() -> list().
make_pool_spec() ->
    Conf = wf:config(sgi, servers),
    make_pool_spec(Conf, []).

-spec make_pool_spec(List, List1) -> list() when
    List :: list(),
    List1 :: list().
make_pool_spec([], New) -> New;
make_pool_spec([H | T], Acc) ->
    P = make_pool_spec(sgi:mv(start_connections, H, 1), H, []),
    make_pool_spec(T, Acc ++ P).

-spec make_pool_spec(Num, Conf, List) -> List1 when
    Num  :: non_neg_integer(),
    Conf :: [tuple()],
    List :: [],
    List1 :: [].
make_pool_spec(0, _, L) -> L;
make_pool_spec(Num, Conf, L) when is_integer(Num) andalso Num > 0 ->
    N = wf:to_atom("Pool#" ++ wf:to_list(sgi:mv(name, Conf, default)) ++ "," ++ wf:to_list(rand:uniform(100000))),
    M = #{id => N, start => {sgi_pool, start_link, [N, Conf]}},
    make_pool_spec(Num - 1, Conf, [M | L]);
make_pool_spec(_, _, _) -> [].

