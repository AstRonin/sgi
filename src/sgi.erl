-module(sgi).

-compile (export_all).

%% API
-export([]).

-spec pv(Key, List, Default) -> term() when
    Key :: term(),
    List :: [term()],
    Default :: term().
pv(K, L) ->
    pv(K, L, undefined).
pv(K, L, D) ->
    proplists:get_value(K, L, D).

-spec is_alive(Pid :: pid()) -> boolean().
is_alive(Pid) when is_pid(Pid) ->
    is_process_alive(Pid);
is_alive(ProcName) when is_atom(ProcName) andalso ProcName =/= undefined ->
    is_alive(whereis(ProcName));
is_alive(_) ->
    false.