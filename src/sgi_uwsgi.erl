-module(sgi_uwsgi).

%%-behavior(gateway_behaviour).
-behaviour(gen_server).

%% API
-export([start/0, stop/1, init_uwsgi/0, end_uwsgi/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(MODIFIER1_BASE, 0).
-define(MODIFIER1_PING, 100).

-define(MODIFIER2_BASE, 0).
-define(MODIFIER2_PONG, 1).

-define(ARBITER, sgi_arbiter).
-define(REQUEST_ID_MAX, 65535).

-record(state, {parent :: pid(),
                pool_pid :: pid(),
                buff = <<>> :: binary()}).

%%%===================================================================
%%% export
%%%===================================================================

start() ->
    gen_server:start(?MODULE, [], []).
stop(Pid) ->
    case sgi:is_alive(Pid) of
        true -> gen_server:stop(Pid);
        _ -> ok
    end.

init_uwsgi() ->
    ok.

end_uwsgi() ->
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

%%=============================================================
%% Send management request and getting connection data of server
%%=============================================================
%%handle_call(?FCGI_GET_VALUES, {_From, _Tag}, State) ->
%%    Data = encode(?MODIFIER1_PING, ?MODIFIER2_BASE, <<>>),
%%
%%    wf:info(?MODULE, "Send test data to uwsgi: ~p~n", [Data]),
%%
%%    Ret = sgi_pool:once_call(Data),
%%    wf:info(?MODULE, "Request Configuration return: ~p~n", [Ret]),
%%    {reply, {ok, []}, State};

%%===================================
%% Send data
%%===================================

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% This event send a message by one request, what is much faster than separately
%% @todo Need some refactoring...!!!
handle_info({overall, From, Params, HasBody, Body}, State) ->
    case ?ARBITER:alloc() of
        {ok, PoolPid} ->
%%            Data1 = encode(99, 0, <<>>),
            Data = encode(?MODIFIER1_BASE, ?MODIFIER2_BASE, encode_pairs(Params)), % @todo What is modifier1 - 30 ???
            case HasBody of
                true ->
                    PoolPid ! {send, <<Data/binary, Body/binary>>, self()};
                _ ->
                    PoolPid ! {send, Data, self()}
            end,
            {noreply, State#state{parent = From, pool_pid = PoolPid}};
        {error, _Reason} ->
            {noreply, State}
    end;


%%===================================
%% Receive data
%%===================================

handle_info({socket_return, Data}, State) ->
    State1 = send_back(Data, State),
    {noreply, State1};
handle_info({socket_error, Data}, State) ->
    State#state.parent ! {sgi_uwsgi_return_error, Data},
    {noreply, State};


%%===================================
%% Other methods
%%===================================

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.pool_pid of undefined -> ok; Pid -> ?ARBITER:free(Pid) end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec encode(Modifier1 :: integer(), Modifier2 :: integer(), binary()) -> binary().
encode(Modifier1, Modifier2, Content = <<>>) ->
    encode_add_header(Modifier1, Modifier2, Content);
encode(Modifier1, Modifier2, Content) ->
    encode(Modifier1, Modifier2, iolist_to_binary(Content), []).
encode(_, _, <<>>, Bin) ->
    iolist_to_binary(lists:reverse(Bin));
encode(Modifier1, Modifier2, Content, Bin) ->
    encode(Modifier1, Modifier2, <<>>, [encode_add_header(Modifier1, Modifier2, Content) | Bin]).
encode_add_header(Modifier1, Modifier2, Content) ->
    <<Modifier1, (iolist_size(Content)):16/little, Modifier2, Content/binary>>.

-spec encode_pairs(list()) -> list().
encode_pairs(P) ->
    encode_pairs(P, []).
encode_pairs([H|T], Res) ->
    encode_pairs(T, [encode_pair(H)|Res]);
encode_pairs([], Res) ->
    lists:reverse(Res).
encode_pair({N, V}) ->
    iolist_size(N),
    <<(iolist_size(N)):16/little, N/binary, (iolist_size(V)):16/little, V/binary>>.

send_back(<<>>, State) ->
    State;
send_back(Data, State) ->
    State#state.parent ! {sgi_uwsgi_return, Data},
    State.

decode_pairs(B) -> decode_pairs(B, []).
decode_pairs(<<>>, Pairs) -> lists:reverse(Pairs);
decode_pairs(B, Pairs) ->
    {ok, {K,V}, B1} = decode_struct(B),
    decode_pairs(B1, [{K, V} | Pairs]).

decode_struct(<<L:16/little,  B/binary>>) ->
    <<K:L/binary, B1/binary>> = B,
    decode_struct(B1, K).
decode_struct(<<L:16/little,  B/binary>>, K) ->
    <<V:L/binary, B1/binary>> = B,
    {ok, {K,V}, B1}.
