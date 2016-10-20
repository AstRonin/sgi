-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

-define(ARBITER, sgi_arbiter).


-define(CLIENTS_COUNT, 20).
-define(REQUESTS_COUNT, 100).
-define(SERVER_DISTRIBUTION_PRIORITY_FILE, "server_distribution(priority).csv").
-define(SERVER_DISTRIBUTION_BLURRED_FILE, "server_distribution(blurred).csv").

start() ->
    start_stat(),
    % waiting servers
    timer:sleep(2000),
    start_client(?CLIENTS_COUNT, 3).

start_stat() ->
    try
        unregister(stat_proc),
        exit(whereis(stat_proc))
    catch _:_  -> ok end,
    Pid = spawn(?MODULE, stat, []),
    true = register(stat_proc, Pid).

stat() ->
    FileName = case wf:config(sgi, balancing_method) of
        blurred -> ?SERVER_DISTRIBUTION_BLURRED_FILE;
        _ -> ?SERVER_DISTRIBUTION_PRIORITY_FILE
    end,
    file:write_file(FileName, <<>>, []),
    stat([]).
stat(Stat) ->
    receive
        {port, P} -> stat([P|Stat]);
        {client_stoped, ok} ->
            case Stat of
                [] -> skip;
                StatData ->
                    % save data to the file for generating graphs
                    Bytes = wf:to_binary(string:join([wf:to_list(E) || E <- lists:reverse(StatData)], "\n") ++ "\n"),
                    FileName = case wf:config(sgi, balancing_method) of
                        blurred -> ?SERVER_DISTRIBUTION_BLURRED_FILE;
                        _ -> ?SERVER_DISTRIBUTION_PRIORITY_FILE
                    end,
                    file:write_file(FileName, Bytes, [append])
            end,
            stat([])
    end.

start_client(0, _) ->
    wf:info(?MODULE,"Cliends started~n", []),
    ok;
start_client(N,Doubled) ->
    spawn(?MODULE, client, [N]),
    case Doubled of
        3 when N == trunc(?CLIENTS_COUNT*0.1) -> timer:sleep(100), start_client(?CLIENTS_COUNT*2, 2);
        2 when N == trunc(?CLIENTS_COUNT*0.1) -> timer:sleep(300), start_client(?CLIENTS_COUNT*3, 1);
        _ -> start_client(N-1, Doubled)
    end.

client(N) ->
    erlang:yield(),
    send(?REQUESTS_COUNT, N).

send(0, _) ->
    stat_proc ! {client_stoped, ok},
    wf:info(?MODULE,"Cliend stoped~n", []),
    ok;
send(N, ClientNum) ->
    {ok, PoolPid} = ?ARBITER:alloc(),
    PoolPid ! {send, <<"Hi server">>, self()},
    receive {socket_return, <<P:5/binary, _:3/binary, _/binary>>} -> stat_proc ! {port, P} after 10000 -> ok end,
    ?ARBITER:free(PoolPid),
    send(N-1, ClientNum).