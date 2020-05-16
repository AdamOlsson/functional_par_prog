-module(throttle).
-compile(export_all).

start() ->
  spawn_link(fun() ->
                 process_flag(trap_exit,true),
                 register(throttle,self()),
                 throttle([])
             end).

throttle(Workers) ->
  receive
    {speculate,Client,Ref} ->
      Limit = erlang:system_info(schedulers) - 1,
      if length(Workers) < Limit ->
          W = spawn_link(fun()-> worker(Client,Ref) end),
          Client ! {speculate,W,Ref},
          throttle([W|Workers]);
        true ->
          Client ! {no_speculation,Ref},
          throttle(Workers)
      end;
    {'EXIT',Pid,_Reason} ->
      case lists:member(Pid,Workers) of
        true ->
          throttle(lists:delete(Pid,Workers));
        false ->
          %% termination of the process that started the throttle
          ok
      end
  end.

worker(Client,Ref) ->
  receive
    {work,F,Ref} ->
      Client ! {value,F(),Ref}
  end.    

speculate(F) ->
  Ref = make_ref(),
  throttle ! {speculate,self(),Ref},
  receive
    {speculate,W,Ref} ->
      link(W),
      W ! {work,F,Ref},
      {speculating,W,Ref};
    {no_speculation,Ref} ->
      {not_speculating,F}
  end.

value_of({speculating,_Pid,Ref}) ->
  receive {value,X,Ref} ->
      X
  end;
value_of({not_speculating,F}) ->
  F().

cancel({speculating,Pid,_Ref}) ->
  unlink(Pid),
  exit(Pid,kill);
cancel({not_speculating,_F}) ->
  ok.

