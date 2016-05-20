-module(sgi_sup).
-behaviour(supervisor).

-export([start_link/0, start_child_pool/3, start_child/1, stop_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child_pool(atom(), ex_fcgi:address(), ex_fcgi:port_number()) ->
    {ok, pid()} | {error, term()}.
start_child_pool(Name, Address, Port) ->
    ChildSpec = {Name,
        {ex_fcgi, start_link, [Name, Address, Port]},
        permanent, 5000, worker, [ex_fcgi]},
    supervisor:start_child(?MODULE, ChildSpec).

-spec start_child(atom()) ->
    {ok, pid()} | {error, term()}.
start_child(Name) ->
    ChildSpec = {Name,
        {Name, start_link, [Name]},
        permanent, 5000, worker, [Name]},
    supervisor:start_child(?MODULE, ChildSpec).

-spec stop_child(atom()) -> ok | {error, term()}.
stop_child(Name) ->
    case supervisor:terminate_child(?MODULE, Name) of
        ok -> supervisor:delete_child(?MODULE, Name);
        Error -> Error end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [#{id => sgi_arbiter,
                    start => {sgi_arbiter, start_link, [self()]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [sgi_arbiter]}],



    {ok, {SupFlags, ChildSpecs}}.
%%    {ok, {{one_for_one, 5, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
