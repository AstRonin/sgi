%%===================
%% @TODO
%% 1. Check max connections(children) and hibernate(or close) extra processes.
%% 2. Explode content to several parts if it bigger then FCGI_MAX_CONTENT_LEN (65535 bytes)
%%===================


-module(sgi_fcgi).
%%-behavior(gateway_behaviour).
-behaviour(gen_server).

%% API
-export([request_pid/1]).

-export([start/0, stop/1, init_fcgi/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(ARBITER, sgi_arbiter).
-define(CONTROLLER, sgi_fcgi_controller).
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


-record(state, {parent, role = ?FCGI_RESPONDER, req_id, pool_pid, out = <<>>, err = <<>>}).

-record(sgi_fcgi_requests, {req_id, pid, timer}).
-record(sgi_fcgi_request_id, {req_id}).

%%%===================================================================
%%% export
%%%===================================================================


%%start_link() ->
%%    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

request_pid(Data) ->
    case erlang:decode_packet(fcgi, Data, []) of
        {ok, <<?FCGI_VERSION_1, _Type, ReqId:16, _>>, _Rest} ->
            [Req] = find_req(ReqId),
            Req#sgi_fcgi_requests.pid;
        {more, _More} -> % undefined
            undefined
    end.

init_fcgi() ->
    ets:new(?REQUESTS, [named_table, {keypos, #sgi_fcgi_requests.req_id}]),
    ets:new(?REQUEST_ID, [named_table]),
    ets:insert(?REQUEST_ID, {req_id, 0}),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

% Send first request
% {?FCGI_BEGIN_REQUEST, ?FCGI_RESPONDER, ?FCGI_KEEP_CONN}
handle_call({?FCGI_BEGIN_REQUEST, Role, KeepConn}, From, State) ->
    R = req_id(),
    save_req(R),

    Data = encode(?FCGI_BEGIN_REQUEST, R, <<Role:16, KeepConn, 0:40>>),

    {ok, PoolPid} = ?ARBITER:alloc(),
    ?CONTROLLER ! {send, Data, PoolPid},

    case wf:config(sgi, multiplexed) of true -> ?ARBITER:free(PoolPid); _ -> ok end,

    State1 = State#state{parent = From, req_id = R, pool_pid = PoolPid},
    {reply, ok, State1};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({?FCGI_PARAMS, Params}, State) ->
    Data = encode(?FCGI_PARAMS, State#state.req_id, encode_pairs(Params)),
    ?CONTROLLER ! {send, Data, State#state.pool_pid},
    {noreply, State};
% Send body
handle_info({?FCGI_STDIN, Request}, State) ->
    Data = encode(?FCGI_STDIN, State#state.req_id, Request),
    ?CONTROLLER ! {send, Data, State#state.pool_pid},
    {noreply, State};
handle_info(<<>>, State) -> %% send empty string as end of request
    Data = encode(?FCGI_STDIN, State#state.req_id, <<>>),
    ?CONTROLLER ! {send, Data, State#state.pool_pid},
    {noreply, State};
handle_info(?FCGI_ABORT_REQUEST, State) ->
    Data = encode(?FCGI_ABORT_REQUEST, State#state.req_id, <<>>),
    ?CONTROLLER ! {send, Data, State#state.pool_pid},
    {noreply, State};
handle_info(?FCGI_GET_VALUES, State) ->
    Params = [{<<?FCGI_MAX_CONNS>>, <<>>}, {<<?FCGI_MAX_REQS>>, <<>>}, {<<?FCGI_MPXS_CONNS>>, <<>>}],
    Data = encode(?FCGI_GET_VALUES, 0, Params),
    ?CONTROLLER ! {send, Data, State#state.pool_pid},
    {noreply, State};
handle_info({socket_return, Data}, State) ->
    State1 = case decode(Data) of
        {?FCGI_STDERR, E, Rest} ->
            wf:info(?MODULE, "socket_return, FCGI_STDERR: ~p~n", [E]),
            wf:info(?MODULE, "socket_return, FCGI_STDERR, Rest: ~p~n", [Rest]),
            State#state{err = [State#state.err | stream_body(E)]};
        {?FCGI_STDOUT, Packet, Rest} ->
            wf:info(?MODULE, "socket_return, FCGI_STDOUT: ~p~n", [Packet]),
            wf:info(?MODULE, "socket_return, FCGI_STDOUT, Rest: ~p~n", [Rest]),
            State#state{out = [State#state.out | stream_body(Packet)]};
        {?FCGI_END_REQUEST, <<AppStatus:32, ProtocolStatus, _Reserved:24>>, Rest} ->
            wf:info(?MODULE, "socket_return, FCGI_END_REQUEST, AppStatus:~p~n, ProtocolStatus:~p~n", [AppStatus, ProtocolStatus]),
            wf:info(?MODULE, "socket_return, FCGI_END_REQUEST, Rest:~p~n", [Rest]),
            State#state.parent ! sgi_fcgi_return_end,
            State;
        more ->
            State#state.parent ! {sgi_fcgi_return, State#state.out, State#state.err},
            State#state{out = <<>>, err = <<>>}
    end,
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case wf:config(sgi, multiplexed, false) of false -> ?ARBITER:free(State#state.pool_pid); _ -> ok end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%%%===================================================================
%%% Internal functions
%%%===================================================================


req_id() ->
    ets:update_counter(?REQUEST_ID, req_id, {2, 1, ?REQUEST_ID_MAX, 1}).

save_req(ReqId) ->
    ets:insert_new(?REQUESTS, #sgi_fcgi_requests{req_id = ReqId, pid = self()}).

find_req(ReqId) ->
    ets:lookup(?REQUESTS, ReqId).

encode(Type, ReqId, Content) ->
    [<<?FCGI_VERSION_1, Type, ReqId:16, (iolist_size(Content)):16, 0, 0>>] ++ Content.

encode_pairs(P) ->
    encode_pairs(P, []).
encode_pairs([H|T], Res) ->
    encode_pairs(T, [encode_pair(H) | Res]);
encode_pairs([], Res) ->
    Res.
encode_pair({N, V}) ->
    [<<encode_pair_len(N), encode_pair_len(V), N, V>>].
encode_pair_len(D) ->
    case iolist_size(D) of
        L when L < 128 -> <<0:1, L:7>>;
        L1 -> <<1:1, L1:31>>
    end.

decode(Data) ->
    case erlang:decode_packet(fcgi, Data, []) of
    {ok, <<?FCGI_VERSION_1, Type, _ReqId:16, PacketLength:16, _PaddingLength, _Reserved, Packet:PacketLength/binary>>, Rest} ->
        {Type, Packet, Rest};
    {more, _More} ->
        more
    end.

stream_body(<<>>) ->
    eof;
stream_body(Bin) ->
    Bin.