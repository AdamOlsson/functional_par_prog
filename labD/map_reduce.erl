%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a very simple implementation of map-reduce, in both 
%% sequential and parallel versions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(map_reduce).
-compile(export_all).

%% We begin with a simple sequential implementation, just to define
%% the semantics of map-reduce. 

%% The input is a collection of key-value pairs. The map function maps
%% each key value pair to a list of key-value pairs. The reduce
%% function is then applied to each key and list of corresponding
%% values, and generates in turn a list of key-value pairs. These are
%% the result.

map_reduce_seq(Map,Reduce,Input) ->
    Mapped = [{K2,V2}
	      || {K,V} <- Input,
		 {K2,V2} <- Map(K,V)],
    reduce_seq(Reduce,Mapped).

reduce_seq(Reduce,KVs) ->
    [KV || {K,Vs} <- group(lists:sort(KVs)),
	   KV <- Reduce(K,Vs)].

group([]) ->
    [];
group([{K,V}|Rest]) ->
    group(K,[V],Rest).

group(K,Vs,[{K,V}|Rest]) ->
    group(K,[V|Vs],Rest);
group(K,Vs,Rest) ->
    [{K,lists:reverse(Vs)}|group(Rest)].

map_reduce_par(Map,M,Reduce,R,Input) ->
    Parent = self(),
    Splits = split_into(M,Input),
    Mappers = 
	[spawn_mapper(Parent,Map,R,Split)
	 || Split <- Splits],
    Mappeds = 
	[receive {Pid,L} -> L end || Pid <- Mappers],
    Reducers = 
	[spawn_reducer(Parent,Reduce,I,Mappeds) 
	 || I <- lists:seq(0,R-1)],
    Reduceds = 
	[receive {Pid,L} -> L end || Pid <- Reducers],
    lists:sort(lists:flatten(Reduceds)).

spawn_mapper(Parent,Map,R,Split) ->
    spawn_link(fun() ->
			Mapped = [{erlang:phash2(K2,R),{K2,V2}}
				  || {K,V} <- Split,
				     {K2,V2} <- Map(K,V)],
			Parent ! {self(),group(lists:sort(Mapped))}
		end).



split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds) ->
    Inputs = [KV
	      || Mapped <- Mappeds,
		 {J,KVs} <- Mapped,
		 I==J,
		 KV <- KVs],
    spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)} end).


map_reduce_dist(Map,M,Reduce,R,Input) ->
    Splits   = split_into(M,Input),
    Pool     = mypool:pool(),
    Mappeds  = mappers_from_pool(Pool, Map, R, Splits),
    Reduceds = reducers_from_pool(Pool, Reduce, lists:seq(0,R-1), Mappeds),
    lists:sort(lists:flatten(Reduceds)).


map_local(Map, R, Split) -> 
    Mapped = [{erlang:phash2(K2,R),{K2,V2}} || {K,V} <- Split, {K2,V2} <- Map(K,V)],
    group(lists:sort(Mapped)).


mappers_from_pool(_, _, _, []) -> [];
mappers_from_pool(_, Map, R, [Split]) -> map_local(Map, R, Split);
mappers_from_pool(Pool, Map, R, [Split | Splits]) ->
    Pool ! {get_node, self()},
    receive 
        {use_node, Node} ->
            Ref = make_ref(),
            Parent = self(),
            spawn_link(Node, fun() -> 
                Mapped = [{erlang:phash2(K2,R),{K2,V2}} || {K,V} <- Split, {K2,V2} <- Map(K,V)],
                Pool ! {available, Node},
                Parent ! {Ref, group(lists:sort(Mapped))}
            end),
            mappers_from_pool(Pool, Map, R, Splits) ++ receive {Ref, Mapped} -> Mapped end;
        {no_node, _} ->
            map_local(Map, R, Split) ++ mappers_from_pool(Pool, Map, R, Splits)
    end.  


reduce_local(Reduce, R, Mappeds) ->
    Inputs = [KV || {J,KVs} <- Mappeds, R==J, KV <- KVs],
    reduce_seq(Reduce,Inputs).

reducers_from_pool(_, _, [], _) -> [];
reducers_from_pool(_, Reduce, [R], Mappeds) -> reduce_local(Reduce, R, Mappeds);
reducers_from_pool(Pool, Reduce, [R | Rs], Mappeds) ->
    Pool ! {get_node, self()},
    receive 
        {use_node, Node} ->
            Ref = make_ref(),
            Parent = self(),
            Inputs = [KV || {J,KVs} <- Mappeds, R==J, KV <- KVs],
            spawn_link(Node, fun() ->
                Reduced = reduce_seq(Reduce,Inputs),
                Pool ! {available, Node},
                Parent ! {Ref, Reduced}
            end),
            reducers_from_pool(Pool, Reduce, Rs, Mappeds) ++ receive {Ref, Reduced} -> Reduced end;
        {no_node, _} ->
            reduce_local(Reduce, R, Mappeds) ++ reducers_from_pool(Pool, Reduce, Rs, Mappeds)
    end.  


map_reduce_load_balance(Map,M,Reduce,R,Input) ->
    Splits   = split_into(M,Input),
    MapFuns  = lists:map(fun(Split) -> fun() -> 
                                Mapped = [{erlang:phash2(K2,R),{K2,V2}} || {K,V} <- Split, {K2,V2} <- Map(K,V)],
                                group(lists:sort(Mapped))
                            end
                        end, Splits),
    Mappeds = mypool:do_work3(MapFuns),
    ReduceFuns = lists:map(fun(R) -> 
                            Inputs = [KV || {J,KVs} <- Mappeds, R==J, KV <- KVs],
                            fun() ->
                                reduce_seq(Reduce, Inputs)
                            end
                        end, lists:seq(0,R-1)),
    Reduceds = mypool:do_work3(ReduceFuns),
    lists:sort(lists:flatten(Reduceds)).


%  [ {0, [{"00",1},{"00",1},{"01",1},{"11",1},{"12",1},{"12",1},{"21",1},{"12",1},{"42",1},{"32",1},{"22",1}]} ]
% lists:map(fun(X) -> fun() -> X+1 end end, L). 
            