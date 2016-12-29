-module(sgi_n2o_fcgi_handler).
-include_lib("n2o/include/wf.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([init/0, send/0, send/1, do_send/3, stop/0]).

-define(PROTO_CGI, <<"CGI/1.1">>).
-define(PROTO_HTTP, <<"HTTP/1.1">>).
-define(DEF_RESP_STATUS, 200).
-define(DEF_TIMEOUT, 600000).

-define(WS_URL_PARTS, (get(sgi_n2o_fcgi_ws_url_parts))).

-type http() :: #http{}.
-type htuple() :: {binary(), binary()}.

-record(ws_url_parts, {scheme, userInfo, host, port, path, query}).
-record(response_headers, {buff = [], ended = false}).

%% ===========================================================
%% API
%% ===========================================================

-spec init() -> ok | {error, term()}.
init() ->
    ok.

-spec stop() -> ok | {error, term()}.
stop() ->
    ok.

send() -> send(#http{}).

-spec send(http()) -> {binary(), integer(), list()}.
send(Http) ->
    wf:state(status, ?DEF_RESP_STATUS),
    make_ws_url_parts(Http),
    vhosts(),
    {_, Body} = bs(Http),
    CGIParams = get_params(Http),

    Ret = case sgi_cluster:is_use() of
        true ->
            sgi_cluster:send(sgi_n2o_fcgi_handler, do_send, [CGIParams, has_body(Http), Body]);
        _ ->
            do_send(CGIParams, has_body(Http), Body)
    end,

%%    {ok, Pid} = sgi_fcgi:start(),
%%    Pid ! {overall, self(), CGIParams, has_body(Http), Body},
%%    Timer = erlang:send_after(wf:config(sgi, response_timeout, ?DEF_TIMEOUT), self(), {sgi_fcgi_timeout, Pid}),
%%    Ret = ret(),
%%    sgi:ct(Timer),
%%    sgi_fcgi:stop(Pid),

    RetH = get_response_headers(),
    set_header_to_cowboy(RetH, 0),
    terminate(),
    %% @todo Return headers from cgi because cowboy don't give access to resp_headers
    {Ret, wf:state(status), RetH}.
%%    {iolist_to_binary(Ret), wf:state(status), RetH}.

do_send(CGIParams, HasBody, Body) ->
    {ok, Pid} = sgi_fcgi:start(),
    Pid ! {overall, self(), CGIParams, HasBody, Body},
    Timer = erlang:send_after(wf:config(sgi, response_timeout, ?DEF_TIMEOUT), self(), {sgi_fcgi_timeout, Pid}),
    Ret = ret(),
    sgi:ct(Timer),
    sgi_fcgi:stop(Pid),
    iolist_to_binary(Ret).

%% ===========================================================
%% Prepare Request
%% ===========================================================

-spec vhosts() -> ok.
vhosts() ->
    Vs = wf:config(sgi, vhosts, []),
    vhosts(Vs).
vhosts([H|T]) ->
    S = sgi:pv(server_name, H, ""),
    A = sgi:pv(aliase, H, ""),
    case wf:to_list(host()) of
        Host when Host =:= S orelse Host =:= A ->
            wf:state(vhost, H), ok;
        _ ->
            vhosts(T)
    end;
vhosts([]) -> wf:state(vhost, []), ok.

-spec vhost(atom()) -> term().
vhost(Key) -> vhost(Key, "").
-spec vhost(atom(), []) -> term().
vhost(Key, Def) ->
    sgi:pv(Key, wf:state(vhost), Def).

-spec get_params(http()) -> list().
get_params(Http) ->
    {{PeerIP, PeerPort}, _} = cowboy_req:peer(?REQ),
    Path = path(),
    QS = qs(),

    Root = vhost(root),
    DefScript = wf:to_binary(vhost(index, "index.php")),
    {FPath, FScript, FPathInfo} = final_path(Path, DefScript),

    %% @todo Waiting of https://github.com/ninenines/cowboy/issues/950
    %% HHttps = case HTTPS ? of <<"https">> -> [{<<"HTTPS">>, <<"'on'">>}]; _ -> [] end,
    HHttps = [],

    [{<<"GATEWAY_INTERFACE">>, ?PROTO_CGI},
        {<<"QUERY_STRING">>, QS},
        {<<"REMOTE_ADDR">>, wf:to_binary(inet:ntoa(PeerIP))},
        {<<"REMOTE_PORT">>, wf:to_binary(PeerPort)},
        {<<"REQUEST_METHOD">>, method(Http)},
        {<<"REQUEST_URI">>, <<Path/binary, (case QS of <<>> -> <<>>; V -> <<"?", V/binary>> end)/binary>>},
        {<<"DOCUMENT_ROOT">>, wf:to_binary(Root)},
        {<<"SCRIPT_FILENAME">>, wf:to_binary([vhost(root), FPath, "/", FScript])},
        {<<"SCRIPT_NAME">>, wf:to_binary(["/", FScript])},
        %% {<<"SERVER_ADDR">>, <<"">>}, % I don't now how cowboy return self ip
        {<<"SERVER_NAME">>, wf:to_binary(vhost(server_name, wf:config(sgi, address, "")))},
        {<<"SERVER_PORT">>, wf:to_binary(port())},
        {<<"SERVER_PROTOCOL">>, ?PROTO_HTTP},
        {<<"SERVER_SOFTWARE">>, <<"cowboy">>}] ++
        path_info_headers(Root, FPathInfo) ++
        HHttps ++
        http_headers(Http) ++
        post_headers(Http).

-spec make_ws_url_parts(http()) -> ok.
make_ws_url_parts(#http{url = undefined}) ->
    wf:state(sgi_n2o_fcgi_ws_url_parts, #ws_url_parts{}), ok;
make_ws_url_parts(#http{url = Url}) ->
    case http_uri:parse(wf:to_list(Url), [{scheme_defaults, [
        {http,80},{https,443},{ftp,21},{ssh,22},{sftp,22},{tftp,69},{ws,80},{wss,443}]}]) of
        {ok, {Scheme, UserInfo, Host, Port, Path, Query}} ->
            R = #ws_url_parts{scheme=Scheme,userInfo=UserInfo,host=Host,port=Port,path=Path,query=Query},
            wf:state(sgi_n2o_fcgi_ws_url_parts, R), ok;
        {error, _Reason} ->
            wf:state(sgi_n2o_fcgi_ws_url_parts, #ws_url_parts{}), ok
    end.

-spec external_headers(http()) -> list().
external_headers(#http{headers = Hs}) ->
    case Hs of undefined -> []; _ -> Hs end.

-spec external_host_header() -> htuple() | undefined.
external_host_header() ->
    case ?WS_URL_PARTS#ws_url_parts.host of
        undefined -> undefined;
        Host ->
            Port = case ?WS_URL_PARTS#ws_url_parts.port of P when P =:= 80 orelse P =:= 443 -> ""; P1 -> ":" ++ wf:to_list(P1) end,
            {<<"host">>, wf:to_binary([Host, Port])}
    end.

-spec external_ajax_header() -> htuple() | undefined.
external_ajax_header() ->
    case ?WS_URL_PARTS#ws_url_parts.host of
        undefined -> undefined;
        _ ->
            {<<"x-requested-with">>, <<"XMLHttpRequest">>}
    end.

-spec post_headers(http()) -> [htuple()] | [].
post_headers(Http) ->
    case has_body(Http) of
        true ->
            [{<<"CONTENT_TYPE">>, <<"application/x-www-form-urlencoded">>},
                {<<"CONTENT_LENGTH">>, wf:to_binary(body_length())}];
        _ -> []
    end.

-spec path_info_headers(string(), string()) -> [htuple()] | [].
path_info_headers(Root, FPathInfo) ->
    case FPathInfo of
        [] -> [];
        _ -> [{<<"PATH_INFO">>, wf:to_binary(FPathInfo)},
            {<<"PATH_TRANSLATED">>, wf:to_binary([Root, FPathInfo])}]
    end.

-spec host() -> binary() | undefined.
host() ->
    case ?WS_URL_PARTS#ws_url_parts.host of
        undefined ->
            case cowboy_req:host_info(?REQ) of
                {undefined, _} -> {H , _} = cowboy_req:host(?REQ), H;
                {H , _} -> H
            end;
        V ->
            V end.

-spec method(http()) -> binary().
method(Http) ->
    case Http#http.method of undefined -> {M, _} = cowboy_req:method(?REQ), M; V -> to_upper(V) end.

-spec has_body(http()) -> true | false.
has_body(Http) ->
    case method(Http) of
        M when M =:= <<"POST">>; M =:= <<"PUT">>; M =:= <<"PATCH">> -> true;
        _ -> false
    end.

-spec body_length() -> non_neg_integer().
body_length() ->
    case wf:state(sgi_n2o_fcgi_body_length) of
        L when is_integer(L) -> L;
        _ ->
            case cowboy_req:body_length(?REQ) of
                {undefined, _} -> 0;
                {CL, _} -> CL
            end
    end.

-spec port() -> inet:port_number().
port() ->
    case ?WS_URL_PARTS#ws_url_parts.port of undefined -> {P, _} = cowboy_req:port(?REQ), P; V -> V end.

-spec path() -> binary().
path() ->
    case ?WS_URL_PARTS#ws_url_parts.path of undefined -> get_cowboy_path(); V -> wf:to_binary(V) end.

-spec qs() -> binary().
qs() ->
    case ?WS_URL_PARTS#ws_url_parts.query of
        undefined -> {Q, _} = cowboy_req:qs(?REQ), Q;
        V -> wf:to_binary(string:strip(V, left, $?))
    end.

-spec bs(http()) -> {ok, binary()} | {empty, <<>>}.
bs(#http{body = undefined}) ->
    case cowboy_req:has_body(?REQ) of
        true ->
            case cowboy_req:body(?REQ) of
                {ok, CB, _} -> {ok, CB};
                _ -> {empty, <<>>}
            end;
        _ -> {empty, <<>>}
    end;
bs(#http{body = B}) when B =:= <<>> orelse B =:= "" ->
    {empty, <<>>};
bs(#http{body = B} = Http) ->
    case has_body(Http) of
        true -> wf:state(sgi_n2o_fcgi_body_length, byte_size(B)), {ok, B};
        _ -> {empty, <<>>}
    end.

-spec http_headers(http()) -> [htuple()].
http_headers(Params) ->
    {H, _} = cowboy_req:headers(?REQ),
    H1 = case external_host_header() of undefined -> H; V -> lists:keystore(<<"host">>, 1, H, V) end,
    H2 = case external_ajax_header() of undefined -> H1; V1 -> lists:keystore(<<"x-requested-with">>, 1, H1, V1) end,
    H3 = H2 ++ external_headers(Params),
    http_headers(H3, []).
http_headers([H|T], New) ->
    K = element(1, H),
    K2 = "HTTP_" ++ string:to_upper(string:join(string:tokens(wf:to_list(K), "-"), "_")),
    http_headers(T, New ++ [{wf:to_binary(K2), element(2, H)}]);
http_headers([], New) ->
    New.

-spec get_cowboy_path() -> binary().
get_cowboy_path() ->
    case cowboy_req:path_info(?REQ) of
        {undefined, _} ->
            case cowboy_req:path(?REQ) of
                {undefined, _} -> <<"/">>;
                {P, _} -> P
            end;
        {P, _} -> <<"/", (binary_join(P, <<"/">>))/binary>>
    end.

-spec final_path(binary(), string()) -> {string(), string(), string()}.
final_path(CowboyPath, DefScript) ->
    {Path, Script, PathInfo} =
        case explode_path(CowboyPath) of
            {path, P} -> {P, DefScript, []};
            {path_parts, P, S, I} -> {P, S, I}
        end,
    {FPath, FScript} =
        case rewrite(Path ++ "/" ++ Script) of
            {true, P1} ->
                case explode_path(P1) of
                    {path, P2} -> {P2, Script};
                    {path_parts, P2, S2, _} -> {P2, S2}
                end;
            {false, _} -> {Path, Script}
        end,
    {FPath, FScript, PathInfo}.

-spec explode_path(Path :: binary()) -> {path, NewPath} | {path_parts, NewPath, Script, PathInfo} when
    NewPath :: string(),
    Script :: string(),
    PathInfo :: string().
explode_path(P) ->
    Path = wf:to_list(P),
    case string:str(Path, ".") of
        0 -> {path, string:strip(Path, right, $/)};
        _ ->
            Tokens = string:tokens(Path, "/"),
            {P1, H, I} = parse_path(Tokens, []),
            {path_parts, P1, H, I}
    end.

-spec parse_path(list(), list()) -> {NewPath, Script, PathInfo} when
    NewPath :: string(),
    Script :: string() | [],
    PathInfo :: string() | [].
parse_path([], []) ->
    {[], [], []};
parse_path([H|T], P) ->
    case string:str(H, ".") of
        0 -> parse_path(T, P ++ "/" ++ H);
        _ -> {P, H, parse_info(T)}
    end;
parse_path([], P) ->
    {P, [], []}.

-spec parse_info(list()) -> string().
parse_info(L) ->
    parse_info(L, []).
parse_info([H|T], I) ->
    parse_info(T, I ++ "/" ++ H);
parse_info([], I) ->
    I.

-spec rewrite(string()) -> {true, string()} | {false, string()}.
rewrite(Subject) ->
    rewrite(Subject, vhost(rewrite, [])).
rewrite(Subject, [RewRule|H]) ->
    case element(1, RewRule) of
        S when S =:= Subject orelse S =:= "*" -> {true, element(2, RewRule)};
        _ -> rewrite(Subject, H)
    end;
rewrite(Subject, []) ->
    {false, Subject}.

%% ===========================================================
%% Response
%% ===========================================================

-spec ret() -> iolist().
ret() ->
    receive
        {sgi_fcgi_return, Out, _Err} ->
            stdout(Out);
        sgi_fcgi_return_end ->
            [];
        {sgi_fcgi_return_error, Err} ->
            wf:error(?MODULE, "Connection error ~p~n", [Err]),
            update_response_headers([{<<"retry-after">>, <<"3600">>}], true),
%%            set_header_to_cowboy([{<<"retry-after">>, <<"3600">>}]),
            wf:state(status, 503),
            [];
        {sgi_fcgi_timeout, Pid} ->
            sgi_fcgi:stop(Pid),
            wf:error(?MODULE, "Connect timeout to FastCGI ~n", []),
            update_response_headers([{<<"retry-after">>, <<"3600">>}], true),
%%            set_header_to_cowboy([{<<"retry-after">>, <<"3600">>}]),
            wf:state(status, 503),
            []
    end.

-spec stdout(list()) -> iolist().
stdout(Data) ->
    case get_response_headers_ended() of
        true -> [Data | ret()];
        _ ->
            {ok, Hs, B} = decode_result(Data),
            case Hs of [] -> skip; _ -> update_response_headers(Hs) end,
            [B | ret()]
    end.

update_response_headers(Hs) ->
    update_response_headers(Hs, false).
update_response_headers(Hs, Status) ->
    RespH1 = case wf:state(sgi_n2o_fcgi_response_headers) of undefined -> #response_headers{}; RespH -> RespH end,
    Status1 = case RespH1#response_headers.ended of true -> true; _ -> Status end,
    RespH2 = RespH1#response_headers{buff = lists:append([Hs, RespH1#response_headers.buff]), ended = Status1},
    wf:state(sgi_n2o_fcgi_response_headers, RespH2).
get_response_headers() ->
    case wf:state(sgi_n2o_fcgi_response_headers) of undefined -> []; H -> lists:reverse(H#response_headers.buff) end.
get_response_headers_ended() ->
    case wf:state(sgi_n2o_fcgi_response_headers) of undefined -> false; H -> H#response_headers.ended end.
get_response_header(K) ->
    sgi:pv(K, get_response_headers(), <<>>).

-spec decode_result(Data) -> {ok, Headers, Body} | {error, term()} when
    Data :: binary(),
    Headers :: list(),
    Body :: binary().
decode_result(<<>>) ->
    {ok, [], <<>>};
decode_result(Data) -> decode_result(Data, []).
decode_result(Data, AccH) ->
    case erlang:decode_packet(httph_bin, Data, []) of
        {ok, http_eoh, Rest} ->
            update_response_headers([], true),
            {ok, AccH, Rest};
        {ok, {http_header,_,<<"X-CGI-",_NameRest>>,_,_Value}, Rest} -> decode_result(Rest, AccH);
        {ok, {http_header,_Len,Field,_,Value}, Rest} -> decode_result(Rest, [{wf:to_binary(Field),Value} | AccH]);
        {ok, {http_error,Value}, Rest} ->
            case get_response_headers() of
                [] ->
                    Rep = binary:replace(Value,<<"\r\n">>,<<"">>),
                    Tokens = string:tokens(wf:to_list(Rep), " "),
                    Status = wf:to_binary(string:join(lists:nthtail(1,Tokens), " ")),
                    decode_result(Rest, [{<<"Status">>,Status} | AccH]);
                _ ->
                    {ok, AccH, Data}
            end;
        {more, undefined} -> {ok, AccH, Data};
        {more, _} -> {ok, [], Data};
        {error, R} -> {error, R}
    end.

set_header_to_cowboy(Hs, _Len) ->
    case Hs of
        undefined -> ok;
        _ ->
            %% @todo Use these default headers, if server will not to do that.
            %% Hs1 = lists:ukeymerge(1, Hs, [{<<"content-length">>, wf:to_binary(Len)}]),
            %% Hs2 = lists:ukeymerge(1, Hs1, [{<<"Content-Type">>,<<"text/html; charset=UTF-8">>}]),
            case wf:state(status) > 500 of
                true -> skip;
                _ ->
                    S = case lists:keyfind(<<"Status">>, 1, Hs) of
                            false -> ?DEF_RESP_STATUS;
                            {_, <<Code:3/binary, _/binary>>} -> wf:to_integer(Code)
                        end,
                    wf:state(status, S)
            end,
            set_header_to_cowboy(Hs)
    end.
set_header_to_cowboy([H|T]) ->
    wf:header(to_lower(element(1,H)), element(2,H)),
    set_header_to_cowboy(T);
set_header_to_cowboy([]) -> ok.

%% ===========================================================
%%
%% ===========================================================

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join([Head|Tail], Sep) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Sep/binary, Value/binary>> end, Head, Tail).

%% @todo If using lib of Cowboy, then...
%% -include_lib("cowlib/include/cow_inline.hrl").
%% to_lower(<<>>, Acc) ->
%%     Acc;
%% to_lower(<<C, Rest/bits>>, Acc) ->
%%     case C of
%%         ?INLINE_LOWERCASE(to_lower, Rest, Acc)
%% end.
-spec to_lower(binary() | string() | atom()) -> binary().
to_lower(Bit) ->
    wf:to_binary(string:to_lower(wf:to_list(Bit))).

-spec to_upper(binary() | string() | atom()) -> binary().
to_upper(V) ->
    wf:to_binary(string:to_upper(wf:to_list(V))).

terminate() ->
    wf:state(vhost, []),
    wf:state(sgi_n2o_fcgi_ws_url_parts, undefined),
    wf:state(sgi_n2o_fcgi_body_length, undefined),
    wf:state(sgi_n2o_fcgi_response_headers, #response_headers{}).
