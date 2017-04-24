# SGI - Socket Gateway Interface

Application is written on Erlang. General design principles are: fast, low memory and modularity.

SGI gives possibility simple and smart way to connect to any server by [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol)
and has other protocols working under TCP.
It supports two protocols:
- [FastCGI](https://en.wikipedia.org/wiki/FastCGI). 
    This protocol is common for connect to [PHP (FPM)](http://php.net/manual/en/install.fpm.php).
- [uwsgi](https://uwsgi-docs.readthedocs.io/en/latest/Protocol.html). 
    This protocol is the native protocol used by the [uWSGI](https://uwsgi-docs.readthedocs.io) server.
    See 3-th sample for more details.

Application be able to create connect through the proxy, now support SOCKS5 protocol, 
and you can send message, for example through the [Tor](https://torproject.org/). 
@see Configuration section.

## Base components

- Server [Cowboy](https://ninenines.eu)
- Framework [n2o](http://synrc.com/apps/n2o/)

## Requirements
   
- Erlang 18.1+

## Documentation

Read documentation in [Wiki](https://github.com/AstRonin/sgi/wiki).

## Upgrade

Read [UPGRADE](https://github.com/AstRonin/sgi/blob/master/UPGRADE.md) before installation new version.

## Try Samples

### Sample 1 - Add a content from other languages inside of your Site.

This sample shows you how you can add "Busines Logic" (big or old) to your site using PHP files.

It is based on the sample from [n2o](https://github.com/synrc/n2o).

    $ git clone git://github.com/astronin/sgi
    $ cd sgi/samples
    $ ./mad deps compile plan
    $ ./mad repl

Run php as fcgi server:

    $ sudo service php5-fpm start

Now you can try it out: http://localhost:8000

### Sample 2 - Your whole common site after erlang server and WebSocket instead of Ajax.

This sample shows you how you can run your site (written in other PL) with support of WebSocket. Forget about Ajax and do your page much more faster.

You have the following advantages compared to Ajax even in common web page:
- Fast
- Low overhead, especially over https
- Easy forwarding a file
- Saving CPU resources of both a client and a server

##### Setup:

    $ git clone git://github.com/astronin/sgi
    $ cd sgi/samples
    
Change app in rebar.config:

    $ vim samples/apps/rebar.config
**{sub_dirs, [ "review" ]}**. -> **{sub_dirs, [ "review2" ]}**.

Change app in sys.config:

**{n2o, [{app,review2}]}**

Run FPM:

    $ sudo service php5-fpm start
    
Run Server:

    $ ./mad deps compile plan
    $ ./mad repl

Url:  http://localhost:8000/site.php

### Sample 3 - Like Sample 2, but using Python.

This sample shows you how you can run your site (wrote in Python and support **uwsgi** protocol) with support of WebSocket.
For this you need to use server [uWSGI](https://uwsgi-docs.readthedocs.io/en/latest/).

##### Setup:

    $ git clone git://github.com/astronin/sgi
    $ cd sgi/samples
    
Change app in rebar.config:

    $ vim samples/apps/rebar.config
**{sub_dirs, [ "review" ]}**. -> **{sub_dirs, [ "review3" ]}**.

Change app in sys.config:

```erlang
{n2o, [{app,review3}]},
{sgi, [{servers, [
    [{name, default}, {address, localhost}, {port, 3031}]
]}]}
```

Run uWSGI:

    $ uwsgi --socket 127.0.0.1:3031 --wsgi-file <your path clone>sgi/samples/cgi-scripts/python/myapp.py

Run Server:

    $ ./mad deps compile plan
    $ ./mad repl

Url:  http://localhost:8000/

### Sample 4 - Part of socket connection

This sample shows you how you can use TCP Client of this app.
Thanks to the smart balancer Client can connect to any number of servers in different methods: `priority` or `blurred`.

##### Setup:

    $ git clone git://github.com/astronin/sgi
    $ cd sgi/samples
    
Change app in rebar.config:

    $ vim samples/apps/rebar.config
**{sub_dirs, [ "review" ]}**. -> **{sub_dirs, [ "review4" ]}**.

Change app in sys.config:

**{n2o, [{app,review4}]}**

Change following settings in `sys.config`. 
Sample will start 10 servers with 5 processes.
Client will connect with 5 sockets on each server.

```erlang
{servers, [
    [{name, default}, {address, localhost}, {port, 10000}, {timeout, 60000}, {weight, 10}, {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa1},    {address, localhost}, {port, 10001}, {timeout, 60000}, {weight, 9},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa2},    {address, localhost}, {port, 10002}, {timeout, 60000}, {weight, 8},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa3},    {address, localhost}, {port, 10003}, {timeout, 60000}, {weight, 7},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa4},    {address, localhost}, {port, 10004}, {timeout, 60000}, {weight, 6},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa5},    {address, localhost}, {port, 10005}, {timeout, 60000}, {weight, 5},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa6},    {address, localhost}, {port, 10006}, {timeout, 60000}, {weight, 4},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa7},    {address, localhost}, {port, 10007}, {timeout, 60000}, {weight, 3},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa8},    {address, localhost}, {port, 10008}, {timeout, 60000}, {weight, 2},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}],
    [{name, aaa9},    {address, localhost}, {port, 10009}, {timeout, 60000}, {weight, 1},  {start_connections, 4}, {max_connections, 5}, {max_fails, 5}, {failed_timeout, 60}]
]},
{balancing_method, priority}, %blurred or priority
```
    
Run Server:

    $ ./mad deps compile plan
    $ ./mad repl

Change in `sys.config` next `{balancing_method, blurred},`

    $ ./mad repl

Sample will create the two files: `server_distribution(priority).csv` and `server_distribution(blurred).csv`
which shows difference between two methods of connection balancing to a server.
Open files in Excel(or other) and insert `XY(Scater)` graph:

![priority](priority1.png)
Below are results with next weights: 

Server port | Weight
--- | ---
10000 | 1
10001 | 2
10002 | 3
10003 | 4
10004 | 10
10005 | 9
10006 | 8
10007 | 7
10008 | 6
10009 | 5
![priority](priority2.png)
![priority](blurred1.png)
