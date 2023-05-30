-module(jobserver).
-compile(export_all).
-define(JOB, {server, 'job@ayman-ZenBook-UX363JA-UX363JA'}).

start() ->
    register(server, spawn_link(?MODULE, init, [])).

stop() ->
    ?JOB ! stop.

init() ->
    loop(#{}).

loop(State) -> 
    receive
        stop ->
            io:format("Jobserver terminated~n");
        {req, Ref, {M, F, A}} -> 
            Worker = spawn_link(fun() -> 
                                ?JOB ! {value, Ref, apply(M, F, A)} 
                                end),
                      loop(maps:put(Ref, started, State));

        {value, Ref, Result} ->  
            loop(maps:update(Ref, Result, State));
            %loop(State#{Ref=>Result});
        {result, Ref, Pid}   -> 
            case maps:is_key(Ref, State) of
                true -> V = maps:get(Ref, State),
                        %io:format(V),
                        case V of
                            started -> Pid ! {inprogress, Ref};
                            Result    -> Pid ! {finished, Result, Ref}
                        end;
                false -> Pid ! {invalid_id, Ref}
            end
    end.


send_request(M, F, A) ->
    Id =make_ref(),
    ?JOB ! {req, Id, {M,F,A}},
    Id.

ask_result(Ref) ->
    ?JOB ! {result, Ref, self()},
    receive
        {inprogress, Ref} -> not_ready_yet;
        {invalid_id, Ref} -> not_avalilable_job;
        {finished, Result, Ref} -> Result
    end.