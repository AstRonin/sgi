-module(sgi_monitoring).

-export([cpu_load/0, mem_load/0, is_critical/0, save_stat/0, do_save_stat/0]).


-define(MAX_FILE_SIZE, 10485760).
-define(MAX_LOG_FILES, 2).
-define(CPU_STAT, "stat/cpu_stat.log").
-define(MEM_STAT, "stat/mem_stat.log").

is_critical() -> cpu_load() > 90 orelse mem_load() > 90.

cpu_load() ->
    D = 50,
    Load = cpu_sup:avg1(),
    100 * (1 - D/(D + Load)).

mem_load() ->
    Ma = mem_allocated(),
    Mt = sgi:pv(total_memory, memsup:get_system_memory_data(), 0),
    Ma / Mt * 100.

mem_allocated() ->
    lists:foldl(fun({_,X}, Sum) -> X + Sum end, 0, erlang:memory()).

save_stat() ->
    spawn(fun() -> do_save_stat() end),
    ok.

do_save_stat() ->
    archive_log(?CPU_STAT),
    archive_log(?MEM_STAT),
    Cpu = cpu_load(),
    Mem = mem_allocated(),
%%    Mem = mem_load(),
    file:write_file(?CPU_STAT, <<(wf:to_binary(Cpu))/binary, "\n">>, [append]),
    file:write_file(?MEM_STAT, <<(wf:to_binary(Mem))/binary, "\n">>, [append]),
    timer:sleep(60000),
    do_save_stat().


archive_log(Filename) ->
    case filelib:file_size(Filename) of
        Size when Size >= ?MAX_FILE_SIZE ->
            archive_log(Filename, ?MAX_LOG_FILES);
        _ ->
            ok
    end.
archive_log(_, -1) ->
    ok;
archive_log(Filename, N) ->
    RotateFile = Filename ++ (case N =:= 0 of true -> ""; _ -> wf:to_list(N) end),
    case filelib:is_file(RotateFile) of
        true ->
            case N =:= ?MAX_LOG_FILES of
                true ->
                    file:delete(RotateFile);
                _ ->
                    file:copy(RotateFile, Filename ++ wf:to_list(N + 1)),
                    case file:open(RotateFile, [write]) of
                        {ok, Handle} ->
                            file:truncate(Handle),
                            file:close(Handle);
                        E2 ->
                            E2
                    end
            end;
        _ ->
            ok
    end,
    archive_log(Filename, N - 1).