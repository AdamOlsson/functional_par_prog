
-module(mypool).
-compile(export_all).

% start_pool() ->
%     spawn_link(fun() ->
%                     Nodes = [ node() | nodes() ],
%                     process_flag(trap_exit,true),
%                     register(pool,self()),
%                     pool(Nodes)
%                 end).

pool() ->
    Nodes = [ node() | nodes() ],
    spawn_link(fun() ->
                pool(Nodes)
                end).

pool([]) ->
    receive 
        {available, Node} ->
            pool([Node]);
        {get_node, Pid} ->
            Pid ! {no_node, Pid},
            pool([])
    end;
pool([Node | Nodes]) -> 
    receive
        {get_node, Pid} ->
            Pid ! {use_node, Node },
            pool(Nodes)
    end.



% do_work([])  -> [];
% do_work([F]) -> F();
% do_work([F | Fs]) ->
%     pool ! {get_node, self()},
%     receive
%         {use_node, Node} ->
%             Ref = make_ref(),
%             Parent = self(),
%             spawn_link(Node, fun() ->
%                 Results = F(),
%                 Parent ! {Ref, Results},
%                 pool ! {available, Node}
%             end),
%             do_work(Fs) ++ receive {Ref, Results} -> Results end;
%         {no_node, _} ->
%             F() ++ do_work(Fs)
%     end.






start_pool2() ->
    spawn_link(fun() ->
                    Nodes = [ node() | nodes() ],
                    Workers = [ spawn_link(Node, fun() ->  worker() end) ||
                                    Node <- Nodes,
                                    _ <- lists:seq(0,10)],
                    process_flag(trap_exit,true),
                    register(pool2,self()),
                    pool2(Workers)
                end).



pool2([]) ->
    receive 
        {available, Worker} ->
            pool2([Worker])
    end;
pool2([Worker | Workers]) ->
    receive 
        {req_worker, Pid} ->
            Pid ! {use_worker, Worker},
            pool2(Workers)
    end.


do_work2([])  -> [];
do_work2([F | Fs]) ->
    pool2 ! {req_worker, self()},
    receive
        {use_worker, Worker} ->
            Ref = make_ref(),
            Client = self(),
            Worker ! {work, F, Client, Ref},
            receive {Ref, Results} -> Results end
    end.


worker() ->
    receive 
        {work, F, Client, Ref} ->
            Client ! {Ref, F()},
            pool2 ! {available, self()},
            worker()
    end.