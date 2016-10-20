-module(review4).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).

main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,review4},review4,[]).
stop(_)    -> ok.

init([]) -> servers:start(),
            timer:sleep(1000),
            index:start(),
            {ok, {{one_for_one, 5, 10}, []}}.
