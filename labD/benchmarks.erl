-module(benchmarks).
-compile(export_all).

-define(EXECUTIONS,1).

map_reduce_par()            -> benchmark(page_rank_par).
map_reduce_dist()           -> benchmark(page_rank_dist).
map_reduce_load_balance()   -> benchmark(page_rank_load_balance).

benchmark(F) -> lists:sum(lists:flatten(lists:map(fun(_) -> 
                                            [T || {T, _} <- [timer:tc(page_rank, F, [])]]
                                        end, lists:seq(0,?EXECUTIONS))))/?EXECUTIONS.




