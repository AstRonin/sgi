-module(sgi_socks5).

%% API
-export([connect/3]).

-define(USERNAME, wf:to_binary(sgi:mv(username, wf:config(sgi, proxy), <<>>))).
-define(PASSWORD, wf:to_binary(sgi:mv(password, wf:config(sgi, proxy), <<>>))).
-define(TIMEOUT, sgi:mv(timeout, wf:config(sgi, proxy), 60000)).

-type port_number() :: 0..65535.
-type socket() :: port().

-spec connect(Socket, DstAddress, DstPort) ->
    ok | {error, Reason} when
    Socket :: socket(),
    DstAddress :: term(),
    DstPort :: port_number(),
    Reason :: term().

connect(Socket, DstAddress, DstPort) ->
    inet:setopts(Socket, [{active, false}]),
    case check_auth(Socket) of
        true ->
            case do_auth(Socket) of
                ok ->
                    do_conn(Socket, DstAddress, DstPort);
                Error ->
                    Error
            end;
        false ->
            do_conn(Socket, DstAddress, DstPort)
    end.

-spec check_auth(Socket :: socket()) -> boolean().

check_auth(Socket) ->
    case ?USERNAME of
        U when byte_size(U) > 0 ->
            case req(Socket, <<5, 2, 0, 2>>) of %% https://tools.ietf.org/html/rfc1928#section-3
                {ok, <<5, 2>>} -> %% username/password
                    true;
                {ok, <<5, 0>>} ->
                    false
            end;
        _ ->
            req(Socket, <<5, 1, 0>>), %% need because the Robot in server side expects this request
            false
    end.

-spec do_auth(Socket :: socket()) -> ok | {error, term()}.

do_auth(Socket) ->
    U = ?USERNAME,
    P = ?PASSWORD,
    Msg = <<1, (byte_size(U)), U/binary, (byte_size(P)), P/binary>>,
    case req(Socket, Msg) of %% http://rfc2.ru/1929.rfc/original
        {ok, <<1, 0>>} ->
            ok;
        _Error ->
            {error, not_authenticated}
    end.

-spec do_conn(Socket, DstAddress, DstPort) ->
    ok | {error, Reason} when
    Socket :: socket(),
    DstAddress :: term(),
    DstPort :: port_number(),
    Reason :: term().

do_conn(Socket, DstAddress, DstPort) ->
    Msg = <<5, 1, 0, (addr(DstAddress))/binary, DstPort:16>>,
    case req(Socket, Msg) of %% https://tools.ietf.org/html/rfc1928#section-4
        {ok, <<5, 0, 0, _Resp/binary>>} ->
            ok;
        {ok, <<5, _RepCode:8, 0, _Resp/binary>>} = ErrorResp ->
            wf:error(?MODULE, "SOCKS5 return connection error response: ~p~n", [ErrorResp]),
            {error, no_connection};
        Error ->
            Error
    end.

-spec addr(Address :: term()) -> binary().

addr(localhost)       -> <<1, 127, 0, 0, 1>>;
addr("localhost")     -> <<1, 127, 0, 0, 1>>;
addr(<<"localhost">>) -> <<1, 127, 0, 0, 1>>;
addr(Address) when is_tuple(Address) -> ip_prep(Address);
addr(Address) ->
    case inet:parse_address(wf:to_list(Address)) of
        {ok, IPAddress} -> ip_prep(IPAddress);
        {error, einval} -> host_prep(Address)
    end.

-spec ip_prep(tuple()) -> binary().

ip_prep({IP1, IP2, IP3, IP4}) ->
    <<1, IP1, IP2, IP3, IP4>>;
ip_prep({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}) ->
    <<4, IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8>>;
ip_prep(_) ->
    <<>>.

-spec host_prep(Host :: binary()) -> binary().

host_prep(Host) when is_binary(Host) =:= false ->
    host_prep(wf:to_binary(Host));
host_prep(Host) ->
    <<3, (byte_size(Host)), Host/binary>>.

-spec req(Socket, Request) -> {ok, B} | {error, Reason} when
    Socket :: port(),
    Request :: term(),
    B :: binary(),
    Reason :: term().

req(Socket, Request) ->
    ok = gen_tcp:send(Socket, Request),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, B} ->
            {ok, B};
        {error, Reason} ->
            {error, Reason}
    end.

