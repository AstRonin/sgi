-module(sgi_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1, start_child/2, stop_child/1, start_pool_children/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    Ret = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    Ch = supervisor:which_children(?SERVER),
    sgi_sup:start_child(sgi_arbiter, Ch),
    Ret.

start_pool_children(Num, Conf) when is_integer(Num) ->
    ChildSpecs = make_pool_spec(Num, Conf, []),
    start_pool_children(ChildSpecs);
start_pool_children(_,_) -> ok.

start_pool_children([H|T]) ->
    supervisor:start_child(?SERVER, H),
    start_pool_children(T);
start_pool_children([]) -> ok.

start_child(N, A) ->
    supervisor:start_child(?MODULE, #{id => N, start => {N, start_link, [A]}}).
start_child(N) ->
    supervisor:start_child(?MODULE, #{id => N, start => {N, start_link, []}}).
stop_child(Name) ->
    case supervisor:terminate_child(?MODULE, Name) of
        ok -> supervisor:delete_child(?MODULE, Name);
        Error -> Error
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = make_pool_spec(),
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec make_pool_spec() -> list().
make_pool_spec() ->
    Conf = wf:config(sgi, servers),
    make_pool_spec(Conf, []).

make_pool_spec([], New) -> New;
make_pool_spec([H|T], L) ->
    P = make_pool_spec(sgi:pv(start_connections,H,1), H, []),
    make_pool_spec(T, L ++ P).

make_pool_spec(0, _, L) -> L;
make_pool_spec(Num, Conf, L) when is_integer(Num) ->
    N = wf:to_atom("Pool#" ++ wf:to_list(sgi:pv(name, Conf, default)) ++ "," ++ wf:to_list(rand:uniform(100000))),
    M = #{id => N, start => {sgi_pool, start_link, [N, Conf]}},
    make_pool_spec(Num - 1, Conf, [M|L]);
make_pool_spec(_, _, _) -> [].

