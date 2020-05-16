-module(unlimited).
-compile(export_all).

solve([]) ->
    [];
solve([[]|_]) ->
    false;
solve([[L]|Clauses]) ->
    case solve(seq:assume(L,Clauses)) of
        false -> 
            false;
        Solution ->
            [L|Solution]
    end;
solve([[L|Lits]|Clauses]) ->
    Rest = throttle:speculate(fun()-> solve(seq:assume(-L,[Lits|Clauses])) end),
    case solve(seq:assume(L,Clauses)) of
	false ->
	    case throttle:value_of(Rest) of
		false ->
		    false;
		Solution ->
		    [-L|Solution]
	    end;
	Solution ->
            throttle:cancel(Rest),
	    [L|Solution]
    end.

