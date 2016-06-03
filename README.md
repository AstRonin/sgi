# SGI - Socket Gateway Interface

Application written on Erlang. General design principles is fast, low memory and modularity.

SGI give possibility simple and smart way to connect to any server by [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol)
and have other protocols working under TCP, first of all this is [FastCGI](https://en.wikipedia.org/wiki/FastCGI).

## Try Sample
It based on the sample from n2o.

    $ git clone git://github.com/astronin/sgi
    $ cd sgi/samples
    $ ./mad deps compile plan repl

Run php as fcgi server

    $ sudo service php5-fpm start

Now you can try it out: http://localhost:8000

## Basic usage of FastCGI protocol with N2O

Add deps to rebar.config:
```erlang
{sgi, ".*", {git, "git://github.com/astronin/sgi", {tag, "master"}}}
```
Add initialization of fcgi protocol:
```erlang
sgi_fcgi:init_fcgi()
```
Add new **event** in your file (index.erl):
```erlang
event(#http{url = _Url, method = _Method, body = _Body} = Http)
```
Send data to the FastCGI Server:
```erlang
{Ret, Status, Headers} = sgi_n2o_fcgi_handler:send(Http),
```
Using js code for returning data to the browser:
```erlang
wf:wire("http.back('"++wf:to_list(js_escape(Ret))++"', "++wf:to_list(Status)++", "++wf:to_list(jsone:encode(Headers))++")");
```

#### Configuration
```erlang
{sgi, [{port, 9000}, % set port to FastCGI
    {address, localhost},
    {timeout, 60000}, % 1 minute
    {fcgi_timeout, 60000}, % 1 minute
    {max_connections, 10}, % run N processes with 1 connection on each process
    {multiplexed, unknown}, % can be "1" | "0" | unknown
    {vhosts, [
        [
            {server_name, "phphost.com"}, % set your server name(domain), for local tests add line "127.0.0.1 phphost.com" into "/etc/hosts" (in Linux), "C:\Windows\System32\drivers\etc\hosts"(in Windows)
            {aliase, "localhost"},
            {root, "/home/roman/dev/sgi/samples/fcgi-scripts"}, % set you FULL path to your codes
            {index, "index.php"}, % default index file
            {rewrite, [{"*", "index.php"}]}
        ],
        [
            {server_name, "yourhost2.com"}, % set your server name(domain)
            {aliase, "localhost"},
            {root, "/usr/local/www/yourhost2.com"}, % set you full path to your codes
            {index, "index.php"} %% default index file
        ]
    ]}
    ]}
```

## Advanced usage 

Application consists two parts: Protocol Part and Connection Part.

Protocol Part works under TCP connection. For now it included FastCGI implementation.
Connection Part is responsible of forwarding message to server.

### Protocol Part:

#### FastCGI
Implementation of protocol include N2O handler and protocol module. 
Other handlers can be written for other frameworks or servers.

##### N2O FastCGI Handler

@See **Basic usage...**

#### FastCGI Protocol
Module runnable as a process.

Start new fcgi process and start connection with fcgi server
```erlang
{ok, Pid} = sgi_fcgi:start(1,1), % FCGI_RESPONDER, FCGI_KEEP_CONN
```
Send Params to server
```erlang 
sgi_fcgi:params(Pid, FCGIParams),
```
Send data to server
```erlang
Pid ! {5, Body},
```
Send marker that the request is ended
```erlang
sgi_fcgi:end_req(Pid),
```
Stop fcgi process with the release of resources
```erlang
sgi_fcgi:stop(Pid),
```
Receive message with next tags
```erlang
{sgi_fcgi_return, Out, Err}
sgi_fcgi_return_end
sgi_fcgi_timeout % self timeout
```

### Connection Part

#### Arbiter
Arbiter decide which stream is free (available), keeps busy socket,
and it does not allow the use of extra resources.

#### Multiplexer

Run multiplexer if your application support multiplexing. Even if you don't know,
will be better run multiplexer also, it help your application don't be fail.

```erlang
{ok, _Pid} = sgi_sup:start_child(sgi_multiplexer, {?MODULE, request_pid})
```

You should set callback function where multiplexer will getting your local PID 
from a storage(ETS) by request_id of your request: `{?MODULE, request_pid}`
Set callback after start process `sgi_multiplexer:set_callback({M, F})` or 
set callback to each request `{send, Request, PoolPid, M, F}`.

##### Using
To search a free stream and hold found:
```erlang
{ok, PoolPid} = sgi_arbiter:alloc(),
```
Send data with multiplexer:
```erlang
sgi_multiplexer ! {send, Data, PoolPid},
```
Or Send data without multiplexer:
```erlang
PoolPid ! {send, Data, self()},
```
To free the held stream:
```erlang
sgi_arbiter:free(PoolPid),
```