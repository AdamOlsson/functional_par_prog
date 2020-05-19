-module(benchmarks).
-compile(export_all).

-define(EXECUTIONS,5).

par()            -> benchmark(page_rank_par).
dist()           -> benchmark(page_rank_dist).
load_balance()   -> benchmark(page_rank_load_balance).

benchmark(F) -> lists:sum(lists:flatten(lists:map(fun(_) -> 
                                            [T || {T, _} <- [timer:tc(page_rank, F, [])]]
                                        end, lists:seq(0,?EXECUTIONS))))/?EXECUTIONS.




