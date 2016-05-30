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

make_pool_spec() ->
    make_pool_spec(wf:config(sgi, max_connections, 1), []).
make_pool_spec(0, L) ->
    L;
make_pool_spec(Num, L) ->
    N = wf:to_atom("Pool#" ++ wf:to_list(Num)),
    PoolTmp = #{start => {sgi_pool, start_link, [N]}},
    M = maps:put(id, N, PoolTmp),
    make_pool_spec(Num - 1, [M|L]).

