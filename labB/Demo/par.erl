-module(par).
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
    Rest = speculate(fun()-> solve(seq:assume(-L,[Lits|Clauses])) end),
    case solve(seq:assume(L,Clauses)) of
	false ->
	    case value_of(Rest) of
		false ->
		    false;
		Solution ->
		    [-L|Solution]
	    end;
	Solution ->
	    [L|Solution]
    end.

%% Speculation

speculate(F) ->
    Parent = self(),
    Pid = spawn_link(fun() -> Parent ! {self(),F()} end),
    {speculating,Pid}.

value_of({speculating,Pid}) ->
    receive {Pid,X} ->
	    X
    end.