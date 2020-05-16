-module(naive).
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
    [lists:delete(-L,C) || C <- Clauses,
		           not lists:member(L,C)].
