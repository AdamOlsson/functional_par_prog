-module(start).
-compile(export_all).


node() ->
    compile:file(benchmarks),
    compile:file(mypool),
    compile:file(page_rank),
    compile:file(map_reduce),
    ssl:start(),
    inets:start().

% c(benchmarks). c(mypool). c(page_rank). c(map_reduce).