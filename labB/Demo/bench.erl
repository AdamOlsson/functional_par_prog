-module(bench).
-compile(export_all).

%% Time to run Name:solve on Example, in milliseconds, measured over N runs.
mark(Name,Example,N) ->
    P = example(Example),
    io:format("~p: ~p variables, ~p clauses\n",
              [Name,
               length(lists:umerge([lists:usort([abs(V) || V <- C]) || C <- P])),
               length(P)]),
    Times = [begin
		 {T,_} = timer:tc(fun() -> Name:solve(P) end),
                 io:format("."),
		 T
	     end
	     || _ <- lists:seq(1,N)],
    Average = lists:sum(Times) / 1000 / N,
    io:format(" ~p ms\n",[Average]),
    Average.

example(Ex) ->
  {ok,[P]} = file:consult(atom_to_list(Ex)++".txt"),
  P.

marks(Names,Example,N) ->
  io:format("Benchmarking on ~p schedulers\n",[erlang:system_info(schedulers)]),
  [mark(Name,Example,N) || Name <- Names].

marks(Example,N) ->
  [throttle:start() || whereis(throttle)==undefined],
  marks([seq,par,limit,limit_cancel,throttled],Example,N).
