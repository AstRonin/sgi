-module(config).
-compile(export_all).

log_level() -> info.
log_modules() -> % any
  [
%    wf_convert,
%    n2o_file,
    n2o_http,

    sgi_n2o_fcgi_handler,
    sgi_pool,
    sgi_fcgi,
    sgi_sup,
    sgi_arbiter,
    sgi_socks5,
    sgi_monitoring,
    sgi_cluster,

%%    n2o_async,
%%    n2o_proto,
%    n2o_client,
%    n2o_static,
%%    n2o_stream,
%%    n2o_nitrogen,
%    n2o_session,
%%    doc,
%%    kvs,
%%    store_mnesia,
    index,
    'index-ws'
  ].
