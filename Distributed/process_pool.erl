-module(process_pool).
-compile(export_all).
-define(LIB, {process_pool,'lib@ayman-ZenBook-UX363JA-UX363JA'}).

create() ->
    register(process_pool, spawn_link(?MODULE, supervisor/0)).

worker()->
    receive
        {{JobFun, Args}, Ticket, Customer} ->
            Result = apply(JobFun, Args),
            Customer ! {done, Ticket, Result},
            ?LIB ! {done, self()},
            worker();
        {JobFun, Ticket, Customer} ->
            Result = apply(JobFun, []),
            Customer ! {done, Ticket, Result},
            ?LIB ! {done, self()},
            worker()
    end.

supervisor(FreeWorkers, BusyWorkers) ->
    supervisor(FreeWorkers, BusyWorkers, []).

supervisor(FreeWorkers, BusyWorkers, CustomerJobs) ->
    receive
        {do_work, {Job, Ticket, Customer}} ->
            case FreeWorkers of
                [Worker | FreeWorkersRest] ->
                    Worker ! {Job, Ticket, Customer},
                    supervisor(FreeWorkersRest, [Worker | BusyWorkers], [{Ticket, Customer, Worker} | CustomerJobs]);
                [] ->
                    supervisor(FreeWorkers, BusyWorkers, [{Ticket, Customer, Job} | CustomerJobs])
            end;
        {done, Worker} ->
            case lists:keyfind(Worker, 2, BusyWorkers) of
                false -> supervisor(FreeWorkers, BusyWorkers, CustomerJobs);
                {_, WorkerPid} ->
                    NewBusyWorkers = lists:keydelete(Worker, 2, BusyWorkers),
                    case CustomerJobs of
                        [{Ticket, Customer, Job} | CustomerJobsRest] ->
                            WorkerPid ! {Job, Ticket, Customer},
                            supervisor(FreeWorkers, [WorkerPid | NewBusyWorkers], CustomerJobsRest);
                        [] ->
                            supervisor([WorkerPid | FreeWorkers], NewBusyWorkers, [])
                    end
            end;
        {'DOWN', _Ref, process, Worker, Reason} ->
            case lists:keyfind(Worker, 3, CustomerJobs) of
                false -> supervisor(FreeWorkers, BusyWorkers, CustomerJobs);
                {Ticket, Customer, _Job} ->
                    Customer ! {worker_error, Ticket, Reason},
                    supervisor(FreeWorkers, BusyWorkers, lists:keydelete(Worker, 3, CustomerJobs))
            end;
        {destroy, Sender} ->
            lists:foreach(fun(Worker) -> exit(Worker, kill) end, BusyWorkers),
            lists:foreach(fun(Worker) -> exit(Worker, kill) end, FreeWorkers),
            Sender ! destroyed,
            receive
                Msg -> Msg
            end;
        Msg ->
            io:format("Unexpected message: ~p~n", [Msg]),
            supervisor(FreeWorkers, BusyWorkers, CustomerJobs)
    end.

supervisor() ->
    Workers = lists:map(fun() -> spawn_monitor(fun worker/0) end, lists:seq(1, 5)),
    supervisor(Workers, []).

do_work(Job) ->
    Ticket = make_ref(),
    process_pool ! {do_work, {Job, Ticket, self()}},
    Ticket.

do_work(JobFun, Args) ->
    Ticket = make_ref(),
    process_pool ! {do_work, {{JobFun, Args}, Ticket, self()}},
    Ticket.

destroy(Sender) ->
    process_pool ! {destroy, Sender},
    receive
        Msg -> Msg
    end.