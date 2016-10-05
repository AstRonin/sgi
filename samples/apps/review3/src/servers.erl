-module(servers).
-include_lib("n2o/include/wf.hrl").

-export([start/0, server/1]).

-define(SERVER_PORTS, [10000,10001,10002,10003,10004,10005,10006,10007,10008,10009]).

start() ->
    start(?SERVER_PORTS).
start([H|T]) ->
    case gen_tcp:listen(H,[{active, true},binary]) of
        {ok, ListenSock} ->
            wf:info(?MODULE,"server started on port: ~p~n",[H]),
            start_servers(2, ListenSock),
            start(T);
        {error,Reason} ->
            wf:error(?MODULE,"Server has not started: ~p~n",[Reason]),
            {error,Reason}
    end;
start([]) ->
    ok.

start_servers(0,_) ->
    ok;
start_servers(Num, LS) ->
    spawn(?MODULE,server,[LS]),
    start_servers(Num-1, LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            loop(S),
            server(LS);
        _ ->
            ok
    end.

loop(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,_Data} ->
            {ok, P} = inet:port(S),
            P1 = wf:to_binary(P),
            gen_tcp:send(S, <<P1/binary, ", Hi client">>),
            loop(S);
        {tcp_closed,S} ->
            wf:info(?MODULE,"Socket ~w closed [~w]~n",[S,self()]),
            ok
    after 10000 -> stop(S)
    end.

stop(S) ->
    gen_tcp:close(S).