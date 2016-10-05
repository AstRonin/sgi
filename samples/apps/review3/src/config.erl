-module(config).
-compile(export_all).

log_level() -> info.
log_modules() -> % any
  [
    servers,
    sgi_pool,
    sgi_sup,
    sgi_arbiter,
    index
  ].
