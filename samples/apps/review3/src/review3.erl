-module(review3).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-include_lib("kvs/include/user.hrl").

main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,review3},review3,[]).
stop(_)    -> ok.

init([]) -> {ok, {{one_for_one, 5, 10}, [spec()]}}.

spec()   -> ranch:child_spec(http, 100, ranch_tcp, port(), cowboy_protocol, env()).
env()    -> [ { env, [ { dispatch, points() } ] } ].
static() ->   { dir, "apps/review3/priv/static", mime() }.
n2o()    ->   { dir, "deps/n2o/priv",           mime() }.
mime()   -> [ { mimetypes, cow_mimetypes, all   } ].
port()   -> [ { port, wf:config(n2o,port,8000)  } ].

points() -> cowboy_router:compile([{'_', [

    {"/static/[...]",       n2o_static,  static()},
    {"/n2o/[...]",          n2o_static,  n2o()},
    {"/ws/[...]",           n2o_stream,  []},
    {'_',                   n2o_cowboy,  []} ]}]).
