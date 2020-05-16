-module(sudoku).
%-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose

transpose([Row]) ->
    [[X] || X <- Row];
transpose([Row|M]) ->
    [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%% 	    ?FORALL(Mat,matrix(M+1,N+1),
%% 		    transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
    [[A,B,C]|triples(D)];
triples([]) ->
    [].

blocks(M) ->
    Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
    lists:append(
      lists:map(fun(X)->
			lists:map(fun lists:append/1, X)
		end,
		Blocks)).

unblocks(M) ->
    lists:map(
      fun lists:append/1,
      transpose(
	lists:map(
	  fun lists:append/1,
	  lists:map(
	    fun(X)->lists:map(fun triples/1,X) end,
	    triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%% 	    unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
    [X || X <- Row,
	  1 =< X andalso X =< 9].

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

safe(M) ->
    safe_rows(M) andalso
	safe_rows(transpose(M)) andalso
	safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
    Nine = lists:seq(1,9),
    [[if 1=<X, X=<9 ->
	      X;
	 true ->
	      Nine
      end
      || X <- Row]
     || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
    NewM =
	refine_rows(
	  transpose(
	    refine_rows(
	      transpose(
		unblocks(
		  refine_rows(
		    blocks(M))))))),
    if M==NewM ->
	    M;
       true ->
	    refine(NewM)
    end.

refine_rows(M) ->
    lists:map(fun refine_row/1,M).

refine_row(Row) ->
    Entries = entries(Row),
    NewRow =
	[if is_list(X) ->
		 case X--Entries of
		     [] ->
			 exit(no_solution);
		     [Y] ->
			 Y;
		     NewX ->
			 NewX
		 end;
	    true ->
		 X
	 end
	 || X <- Row],
    NewEntries = entries(NewRow),
    %% check we didn't create a duplicate entry
    case length(lists:usort(NewEntries)) == length(NewEntries) of
	true ->
	    NewRow;
	false ->
	    exit(no_solution)
    end.

is_exit({'EXIT',_}) ->
    true;
is_exit(_) ->
    false.

%% is a puzzle solved?

solved(M) ->
    lists:all(fun solved_row/1,M).

solved_row(Row) ->
    lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->		      
    lists:sum(
      [lists:sum(
	 [if is_list(X) ->
		  length(X);
	     true ->
		  0
	  end
	  || X <- Row])
       || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
    Nine = lists:seq(1,9),
    {_,I,J,X} =
	lists:min([{length(X),I,J,X}
		   || {I,Row} <- lists:zip(Nine,M),
		      {J,X} <- lists:zip(Nine,Row),
		      is_list(X)]),
    {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
    SortedGuesses =
	lists:sort(
	  [{hard(NewM),NewM}
	   || NewM <- Ms,
	      not is_exit(NewM)]),
    [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
    update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
    {Pre,[_|Post]} = lists:split(I-1,Xs),
    Pre++[X|Post].

%% prop_update() ->
%%     ?FORALL(L,list(int()),
%% 	    ?IMPLIES(L/=[],
%% 		     ?FORALL(I,choose(1,length(L)),
%% 			     update_nth(I,lists:nth(I,L),L) == L))).

%% solve a puzzle
granularity() -> 2. % CHANGE HERE FOR GRANULARITY
solve(M) ->
    % Solution = solve_refined_server(refine(fill(M))),
    Solution = solve_refined(granularity(), refine(fill(M))), 
    case valid_solution(Solution) of
	    true ->
	        Solution;
	    false ->
	        exit({invalid_solution,Solution})
    end.

% Granny == Granularity
solve_refined(M) -> solve_refined(0, M) .
solve_refined(Granny, M) when Granny =< 0  ->
    case solved(M) of
	    true ->
	        M;
	    false ->
	        solve_one(guesses(M))
    end;
solve_refined(Granny, M) ->
    case solved(M) of
	    true ->
	        M;
	    false ->
            solve_many(Granny-1, guesses(M))
    end.


solve_one([])       -> exit(no_solution);
solve_one([M])      -> solve_refined(M);
solve_one([M|Ms])   ->
    case catch solve_refined(M) of 
	    {'EXIT',no_solution} ->
	        solve_one(Ms);
	    Solution ->
	        Solution
    end.

solve_many(_, [])       -> exit(no_solution);
solve_many(Granny, [M]) -> solve_refined(Granny, M);
solve_many(Granny, Ms)  ->
    Pids = [speculate(fun() -> catch solve_refined(Granny, X) end) || X <- Ms],
    get_value(Pids).


% This code is the remains of trying to parallelize using a server-client approach. This would be the client.
% The server implementation, was a complete copy from John Hughes lecture because we were trying to debug
% our client. It can be found in throttle.erl. This code is only left here for the TA to see that we tried. 
solve_refined_server(M)  ->
    case solved(M) of
	    true ->
	        M;
	    false ->
	        solve_server(guesses(M))
    end.

solve_server([])     -> exit(no_solution);
solve_server([M])    -> solve_refined_server(M);
solve_server([M|Ms]) ->
    Rest = throttle:speculate(fun() -> catch solve_refined_server(M) end),
    case catch solve_server(Ms) of
        {'EXIT', no_solution} -> 
            case throttle:value_of(Rest) of
                {'EXIT', no_solution} -> exit(no_solution);
                Solution -> Solution
            end;
        Solution ->
            throttle:cancel(Rest),
            Solution
    end.


get_value([])           -> exit(no_solution);
get_value([Pid | Pids]) ->
    case value_of(Pid) of
        {'EXIT', no_solution} -> get_value(Pids);
        Solution -> 
            cancel(Pids),
            Solution
    end .

cancel([]) -> ok;
cancel([{speculating, Pid}| Pids]) ->
    unlink(Pid),
    exit(Pid, kill),
    cancel(Pids).

speculate(F) ->
    Parent = self(),
    Pid = spawn_link(fun() -> Parent ! {self(), F()} end),
    {speculating, Pid}.

value_of({speculating, Pid}) ->
    receive {Pid, X} ->
        X
    end.

%% benchmarks

-define(EXECUTIONS,100).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

% Solve each puzzle in parallel
benchmarks_par_prime([]) -> [] ;
benchmarks_par_prime([{Name,M}]) -> [{Name, bm(fun() -> solve(M) end)}];
benchmarks_par_prime([{Name,M} | Ms]) ->
    Parent = self(),
    Ref = make_ref(),
    spawn_link(
        fun() -> 
            Parent ! {Ref, bm(fun() -> solve(M) end)} 
        end
    ),
    benchmarks_par_prime(Ms) ++ [{Name, receive {Ref, T} -> T end}] .
    

benchmarks(Puzzles) ->
    [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

% Best speed-up so far is solving each puzzle in parallel, with additional parallelism in those solving processes.
% This is not the task though
benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,benchmarks,[Puzzles]).


benchmarks_par() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,benchmarks_par_prime,[Puzzles]).     
%% check solutions for validity

valid_rows(M) ->
    lists:all(fun valid_row/1,M).

valid_row(Row) ->
    lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
    valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).

