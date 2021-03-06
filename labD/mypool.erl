
-module(mypool).
-compile(export_all).

% ##### Distribute Map-Reduce
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

% ##### Load balancing (new implementation of the worker pool)

start2() ->
    spawn_link(fun() ->
                    Nodes = [ node() | nodes() ],
                    Workers = [ spawn(Node, fun() ->  worker() end) ||
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


worker() ->
    receive 
        {work, F, Client, Ref} ->
            try % For Fault-tolerance
                Client ! {ok, Ref, F()},
                pool2 ! {available, self()},
                worker()
            catch
                {'EXIT', Why} -> 
                    Client ! {crash, Ref, Why}
            end
    end.

client(Parent, F) ->
    pool2 ! {req_worker, self()},
    receive
        {use_worker, Worker} ->
            Ref = make_ref(),
            Client = self(),
            Worker ! {work, F, Client, Ref},
            receive 
                {ok, Ref, Results} ->
                    Parent ! {self(), Results};
                {crash, Ref, Why} -> % For Fault-tolerance
                    client(Parent, F)
            end
    end.


do_work3(Fs) ->
    Parent = self(),
    Clients = [spawn_link(fun() -> client(Parent, F) end) || F <- Fs],
    [receive {Pid,L} -> L end || Pid <- Clients].


