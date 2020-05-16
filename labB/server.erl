-module(server).
-compile(export_all).

start() ->
    spawn_link(fun() ->
        process_flag(trap_exit, true),
        register(server, self()),
        server([])
    end).

server(Workers) ->
    receive
        {speculate, Client, Ref} ->
            Limit = erlang:system_info(schedulers) -1,
            if length(Workers) < Limit ->
                W = spawn_link(fun() -> worker(Client, Ref) end),
                Client ! {speculate, W, Ref},
                server([W|Workers]);
            true -> 
                Client ! {no_speculate, Ref},
                server(Workers)
            end;
        {'EXIT', Pid, _Reason} ->
            case lists:member(Pid, Workers) of
                true -> server(lists:delete(Pid, Workers));
                false -> ok
            end
    end.


worker(Client, Ref) ->
    receive {work, F, Ref} ->
        Client ! {value, F(), Ref}
    end.

speculate(F) ->
    Ref = make_ref(),
    server ! {speculate, self(), Ref},
    receive
        {speculate, W, Ref} ->
            link(W),
            W ! {work, F, Ref},
            {speculating, W, Ref};
        {no_speculate, Ref} -> 
            {not_speculating, F}
    end.


value_of({not_speculating, F}) -> F();
value_of({speculating, _Pid, Ref}) ->
    receive {value, X, Ref} ->
        X
    end.

cancel({not_speculating, _F}) -> ok;
cancel({speculating, Pid, _Ref}) ->
    unlink(Pid),
    exit(Pid, kill).
