-module(sgi).
-compile(export_all).

-spec pv(Key, List) -> term() when
    Key :: term(),
    List :: [term()].
pv(K, L) -> pv(K, L, undefined).
-spec pv(Key, List, Default) -> term() when
    Key :: term(),
    List :: [term()],
    Default :: term().
pv(K, L, D) -> proplists:get_value(K, L, D).

-spec ct(TimerRef) -> Result | ok when
    TimerRef :: reference(),
    Time :: non_neg_integer(),
    Result :: Time | false.
ct(Timer) ->
    case is_reference(Timer) of true -> erlang:cancel_timer(Timer, [{async, true}, {info, false}]); _ -> ok end.

-spec is_alive(Pid :: pid()) -> boolean().
is_alive(Pid) when is_pid(Pid) -> erlang:is_process_alive(Pid);
is_alive(RegName) when is_atom(RegName) andalso RegName =/= undefined -> is_alive(erlang:whereis(RegName));
is_alive(_) -> false.

-spec time_now() -> non_neg_integer().
time_now() ->
    erlang:system_time(seconds).

inc_state(K, Num) ->
    case wf:state(K) of
        undefined ->
            wf:state(K, Num),
            Num;
        S when is_number(S) ->
            wf:state(K, S+Num),
            S+Num;
        _ -> ok
    end.
