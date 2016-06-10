-module(sgi_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1, start_child/2, stop_child/1]).

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

make_pool_spec([], New) ->
    New;
make_pool_spec([H|T], L) ->
    P = make_pool_spec(proplists:get_value(max_connections,H,1), H, []),
    make_pool_spec(T, L ++ P).

make_pool_spec(0, _, L) ->
    L;
make_pool_spec(Num, Conf, L) ->
    N = wf:to_atom("Pool#" ++ wf:to_list(proplists:get_value(name,Conf,default)) ++ "," ++ wf:to_list(Num)),
    PoolTmp = #{start => {sgi_pool, start_link, [N, Conf]}},
    M = maps:put(id, N, PoolTmp),
    make_pool_spec(Num - 1, Conf, [M|L]).

