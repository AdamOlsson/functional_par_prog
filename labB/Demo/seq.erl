-module(seq).
-compile(export_all).

solve([]) ->
    [];
solve([[]|_]) ->
    false;
solve([[L|Lits]|Clauses]) ->
    case solve(assume(L,Clauses)) of
	false ->
	    case solve(assume(-L,[Lits|Clauses])) of
		false ->
		    false;
		Solution ->
		    [-L|Solution]
	    end;
	Solution ->
	    [L|Solution]
    end.

assume(L,Clauses) ->
    NewClauses =
	[lists:delete(-L,C) || C <- Clauses,
			       not lists:member(L,C)],
    sort_by_length(NewClauses).

sort_by_length(Clauses) ->
    [C || {_,C} <- lists:usort([{length(C),C} || C <- Clauses])].
