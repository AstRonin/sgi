-module(config).
-compile(export_all).

log_level() -> info.
log_modules() -> % any
  [
%    wf_convert,
%    n2o_file,
    n2o_http,

    sgi_pool,
    sgi_sup,
    sgi_arbiter,
    sgi_uwsgi,
    sgi_n2o_uwsgi_handler,

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
