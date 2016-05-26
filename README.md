SGI - Socket Gateway Interface
==============================

Arbiter
-------

Pool
----

Multiplexer
-----------
Run proccess as a child of supervisor:
```erlang
sgi_sup:start_child(sgi_multiplexer);
````
Using:
Make function for getting your local PID from a storage(ETS) by request_id of your previous request. 

`Pid = M:F().`

Two ways setting multiplexer:

1. Call `sgi_multiplexer:set_callback({M, F})` after start process.
2. Add M and F to each request `{send, Request, PoolPid, M, F}`.
