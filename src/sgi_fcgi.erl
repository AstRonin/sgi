-module(sgi_fcgi).

%%-behavior(gateway_behaviour).
-behaviour(gen_server).

%% API
-export([request_pid/1]).

-export([start/0, start/2, params/2, end_req/1, stop/1, init_fcgi/0, fcgi_end/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(ARBITER, sgi_arbiter).
-define(MULTIPLEXER, sgi_multiplexer).
-define(REQUEST_ID_MAX, 65535).

%% Maximum number of bytes in the content portion of a FastCGI record.
-define(FCGI_MAX_CONTENT_LEN, 65535).

-define(FCGI_LISTENSOCK_FILENO, 0).

-define(FCGI_HEADER_LEN,8).

-define(FCGI_VERSION_1, 1).

-define(FCGI_BEGIN_REQUEST, 1).
-define(FCGI_ABORT_REQUEST, 2).
-define(FCGI_END_REQUEST, 3).
-define(FCGI_PARAMS, 4).
-define(FCGI_STDIN, 5).
-define(FCGI_STDOUT, 6).
-define(FCGI_STDERR, 7).
-define(FCGI_DATA, 8).
-define(FCGI_GET_VALUES, 9).
-define(FCGI_GET_VALUES_RESULT, 10).
-define(FCGI_UNKNOWN_TYPE, 11).
-define(FCGI_MAXTYPE, ?FCGI_UNKNOWN_TYPE).

-define(FCGI_NULL_REQUEST_ID, 0).

-define(FCGI_KEEP_CONN, 1).

-define(FCGI_RESPONDER, 1).
-define(FCGI_AUTHORIZER, 2).
-define(FCGI_FILTER, 3).

-define(FCGI_REQUEST_COMPLETE, 0).
-define(FCGI_CANT_MPX_CONN, 1).
-define(FCGI_OVERLOADED, 2).
-define(FCGI_UNKNOWN_ROLE, 3).

-define(FCGI_MAX_CONNS, "FCGI_MAX_CONNS").
-define(FCGI_MAX_REQS, "FCGI_MAX_REQS").
-define(FCGI_MPXS_CONNS, "FCGI_MPXS_CONNS").

-define(REQUESTS, sgi_fcgi_requests).
-define(REQUEST_ID, sgi_fcgi_request_id).


-record(state, {parent :: pid(),
                role = ?FCGI_RESPONDER :: integer(),
                req_id :: integer(),
                pool_pid :: pid(),
%%                multiplexed = unknown :: string() | unknown,
                buff = [] :: iolist()}).

-record(sgi_fcgi_requests, {req_id, pid, timer}).
-record(sgi_fcgi_request_id, {req_id}).

%%%===================================================================
%%% export
%%%===================================================================

start() ->
    gen_server:start(?MODULE, [], []).
start(Role, KeepConn) ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    gen_server:call(Pid, {?FCGI_BEGIN_REQUEST, Role, KeepConn}),
    {ok, Pid}.
stop(Pid) ->
    gen_server:stop(Pid).
params(Pid, P) ->
    Pid ! {?FCGI_PARAMS, P}.
end_req(Pid) ->
    Pid ! <<>>.

init_fcgi() ->
    ets:new(?REQUESTS, [public, named_table, {keypos, #sgi_fcgi_requests.req_id}]),
    ets:new(?REQUEST_ID, [public, named_table]),
    ets:insert(?REQUEST_ID, {req_id, 0}),
    check_multiplex(),
    start_multiplexer(),
    wf:info(?MODULE, "Application evns: ~p~n", [application:get_all_env(sgi)]),
    ok.

fcgi_end() ->
    ets:delete(?REQUESTS),
    ets:delete(?REQUEST_ID),
    wf:info(?MODULE, "fcgi ended: ~n", []),
    ok.

-spec request_pid(Data :: binary()) -> pid() | undefined.
request_pid(Data) ->
    case erlang:decode_packet(fcgi, Data, []) of
        {ok, <<?FCGI_VERSION_1, _Type, ReqId:16, _>>, _Rest} ->
            [Req] = find_req(ReqId),
            Req#sgi_fcgi_requests.pid;
        {more, More} -> % undefined
            wf:error(?MODULE, "!!!!!!!!!!!!!!!!!!!!!!request_pid, exception, MORE!!!!!!!!!!!!!!!!!!!!!!!!!!!: ~p~n", [More]),
            undefined
    end.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

%%=============================================================
%% Send management request and getting connection data of server
%%=============================================================
handle_call(?FCGI_GET_VALUES, {From, _Tag}, State) ->
    Params = [{<<?FCGI_MAX_CONNS>>, <<>>}, {<<?FCGI_MAX_REQS>>, <<>>}, {<<?FCGI_MPXS_CONNS>>, <<>>}],
    Data = encode(?FCGI_GET_VALUES, 0, encode_pairs(Params)),
    {ok, PoolPid} = ?ARBITER:alloc(),
    RetData =
    case sgi_pool:once_call(PoolPid, Data) of
        {ok, Return} -> Return;
        {error, Reason} ->
            wf:error(?MODULE, "Request return error: ~p~n", [Reason]),
            <<>>
    end,
    ?ARBITER:free(PoolPid),
    State1 = State#state{parent = From, req_id = 0, pool_pid = PoolPid},
    case decode(RetData) of
         {?FCGI_GET_VALUES_RESULT, Packet, _Rest} ->
             Pairs = decode_pairs(Packet),
             {reply, {ok, Pairs}, State1};
         _ ->
             {reply, {ok, []}, State1}
    end;

%%===================================
%% Send data
%%===================================
% Send first request
% {?FCGI_BEGIN_REQUEST, ?FCGI_RESPONDER, ?FCGI_KEEP_CONN}
handle_call({?FCGI_BEGIN_REQUEST, Role, KeepConn}, {From, _Tag}, State) ->
    R = req_id(),
    save_req(R),
    Data = encode(?FCGI_BEGIN_REQUEST, R, <<Role:16, KeepConn, 0:40>>),
    {ok, PoolPid} = ?ARBITER:alloc(),
    case wf:config(sgi, multiplexed) of
        "0" -> PoolPid ! {send, Data, self()};
        _ -> ?MULTIPLEXER ! {send, Data, PoolPid}
    end,
    case wf:config(sgi, multiplexed) of "1" -> ?ARBITER:free(PoolPid); _ -> ok end,
    State1 = State#state{parent = From, req_id = R, pool_pid = PoolPid},
    {reply, ok, State1};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info({?FCGI_PARAMS, Params}, State) ->
    P = encode(?FCGI_PARAMS, State#state.req_id, encode_pairs(Params)) ++
        encode(?FCGI_PARAMS, State#state.req_id, <<>>),
    case wf:config(sgi, multiplexed) of
        "0" -> State#state.pool_pid ! {send, P, self()};
        _ -> ?MULTIPLEXER ! {send, P, State#state.pool_pid}
    end,
    {noreply, State};
% Send body
handle_info({?FCGI_STDIN, Request}, State) ->
    case wf:config(sgi, multiplexed) of
        "0" -> State#state.pool_pid ! {send, encode(?FCGI_STDIN, State#state.req_id, Request), self()};
        _ -> ?MULTIPLEXER ! {send, encode(?FCGI_STDIN, State#state.req_id, Request), State#state.pool_pid}
    end,
    {noreply, State};
handle_info(<<>>, State) -> %% send empty string as end of request
    case wf:config(sgi, multiplexed) of
        "0" -> State#state.pool_pid ! {send, encode(?FCGI_STDIN, State#state.req_id, <<>>), self()};
        _ -> ?MULTIPLEXER ! {send, encode(?FCGI_STDIN, State#state.req_id, <<>>), State#state.pool_pid}
    end,
    {noreply, State};
handle_info(?FCGI_ABORT_REQUEST, State) ->
    case wf:config(sgi, multiplexed) of
        "0" -> State#state.pool_pid ! {send, encode(?FCGI_ABORT_REQUEST, State#state.req_id, <<>>), self()};
        _ -> ?MULTIPLEXER ! {send, encode(?FCGI_ABORT_REQUEST, State#state.req_id, <<>>), State#state.pool_pid}
    end,
    {noreply, State};

%%===================================
%% Receive data
%%===================================

handle_info({socket_return, Data}, State) ->
    State1 = back(Data, State),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    del_req(State#state.req_id),
    case wf:config(sgi, multiplexed) of "1" -> ok; _ -> ?ARBITER:free(State#state.pool_pid) end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_multiplex() ->
    case wf:config(sgi, multiplexed) of
        [] ->
            {ok, Pid} = ?SERVER:start(),
            {ok, Ret} = gen_server:call(Pid, ?FCGI_GET_VALUES),
            ?SERVER:stop(Pid),
            MPXS1 = case lists:keyfind(wf:to_binary(?FCGI_MPXS_CONNS), 1, Ret) of
                {_, MPXS} -> wf:to_list(MPXS);
                _ -> unknown
            end,
            application:set_env(sgi, multiplexed, MPXS1);
        _ -> ok
    end.

start_multiplexer() -> % we don't need multiplexer if we don't use multiplex connection
    case wf:config(sgi, multiplexed) of
        V when V == "1"; V == unknown ->
            {ok, _} = sgi_sup:start_child(?MULTIPLEXER, {?MODULE, request_pid}), ok;
        _ ->
            ok
    end.

req_id() ->
    ets:update_counter(?REQUEST_ID, req_id, {2, 1, ?REQUEST_ID_MAX, 1}).
save_req(ReqId) ->
    ets:insert_new(?REQUESTS, #sgi_fcgi_requests{req_id = ReqId, pid = self()}).
find_req(ReqId) ->
    ets:lookup(?REQUESTS, ReqId).
del_req(ReqId) ->
    ets:delete(?REQUESTS, ReqId).

encode(Type, ReqId, Content = <<>>) ->
    encode_add_header(Type, ReqId, Content);
encode(Type, ReqId, Content) ->
    encode(Type, ReqId, iolist_to_binary(Content), []).
encode(_, _, <<>>, Bin) ->
    lists:reverse(Bin);
encode(Type, ReqId, <<Content:(?FCGI_MAX_CONTENT_LEN - 8)/binary, Rest/binary>>, Bin) ->
    encode(Type, ReqId, Rest, [encode_add_header(Type, ReqId, Content) | Bin]);
encode(Type, ReqId, Content, Bin) ->
    encode(Type, ReqId, <<>>, [encode_add_header(Type, ReqId, Content) | Bin]).
encode_add_header(Type, ReqId, Content) ->
    <<?FCGI_VERSION_1, Type, ReqId:16, (iolist_size(Content)):16, 0, 0, Content/binary>>.

-spec encode_pairs(list()) -> list().
encode_pairs(P) ->
    encode_pairs(P, []).
encode_pairs([H|T], Res) ->
    encode_pairs(T, [encode_pair(H)|Res]);
encode_pairs([], Res) ->
    lists:reverse(Res).
encode_pair({N, V}) ->
    NL = encode_pair_len(N),
    VL = encode_pair_len(V),
    <<NL/binary, VL/binary, N/binary, V/binary>>.
encode_pair_len(D) ->
    case iolist_size(D) of
        L when L < 128 -> <<0:1, L:7>>;
        L1 -> <<1:1, L1:31>>
    end.

back(Data, State = #state{buff = B}) ->
    case decode(iolist_to_binary(lists:reverse([Data | B]))) of
        {?FCGI_STDERR, E, Rest} ->
            wf:info(?MODULE, "socket_return, FCGI_STDERR: ~p~n", [E]),
            State#state.parent ! {sgi_fcgi_return, <<>>, stream_body(E)},
            back(Rest, State#state{buff = []});
        {?FCGI_STDOUT, Packet, Rest} ->
            State#state.parent ! {sgi_fcgi_return, Packet, <<>>},
            back(Rest, State#state{buff = []});
        {?FCGI_DATA, Packet, Rest} ->
            State#state.parent ! {sgi_fcgi_return, Packet, <<>>},
            back(Rest, State#state{buff = []});
        {?FCGI_END_REQUEST, <<_AppStatus:32, _ProtocolStatus, _Reserved:24>>, Rest} ->
%%            wf:info(?MODULE, "socket_return, FCGI_END_REQUEST, AppStatus:~p~n, ProtocolStatus:~p~n", [AppStatus, ProtocolStatus]),
            State#state.parent ! sgi_fcgi_return_end,
            del_req(State#state.req_id),
            back(Rest, State#state{buff = [], req_id = 0});
        more ->
            State#state{buff = [Data | B]};
        <<>> ->
            State
    end.

decode(<<>>) ->
    <<>>;
decode(Data) ->
    case erlang:decode_packet(fcgi, Data, []) of
    {ok, <<?FCGI_VERSION_1, Type, _ReqId:16, PacketLength:16, _PaddingLength, _Reserved, Packet:PacketLength/binary>>, Rest} ->
        {Type, Packet, Rest};
    {more, undefined} ->
        more;
    {more, _More} ->
        more
    end.

decode_pairs(B) -> decode_pairs(B, []).
decode_pairs(<<>>, Pairs) -> lists:reverse(Pairs);
decode_pairs(<<0:1, NL:7,  0:1, VL:7,  B/binary>>, Pairs) -> decode_pairs(NL, VL, B, Pairs);
decode_pairs(<<1:1, NL:31, 0:1, VL:7,  B/binary>>, Pairs) -> decode_pairs(NL, VL, B, Pairs);
decode_pairs(<<0:1, NL:7,  1:1, VL:31, B/binary>>, Pairs) -> decode_pairs(NL, VL, B, Pairs);
decode_pairs(<<1:1, NL:31, 1:1, VL:31, B/binary>>, Pairs) -> decode_pairs(NL, VL, B, Pairs).
decode_pairs(NL, VL, B, Pairs) ->
    <<N:NL/binary, V:VL/binary, T/binary>> = B,
    decode_pairs(T, [{N, V} | Pairs]).

stream_body(<<>>) ->
    eof;
stream_body(Bin) ->
    Bin.