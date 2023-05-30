-module(jobserver2).
%% Server interface
-export([start/1, stop/0, init/1]).
%% Client interface
-export([send_request/3, ask_result/1]).

-define(SRV, {js, 'srv@ayman-ZenBook-UX363JA-UX363JA'}).

start(Max) ->
    register(js, spawn_link(?MODULE, init, [Max])).

stop() ->
    ?SRV ! stop.

init(_Max) ->
    process_flag(trap_exit, true),
    loop(#{}).

loop(State) ->
    receive
        stop ->
            io:format("Jobserver terminated~n");
        {request, Ref, {M, F, A}} ->
            Worker = spawn_link(fun() -> ?SRV ! {value, Ref, apply(M, F, A)} end),
            loop(State#{Ref=>{Worker, started}});
        {result, Ref, Pid} ->
            case State of
                #{Ref:={_, started}} -> Pid ! {in_progress, Ref};
                % #{Ref := error} -> ...
                #{Ref:=Value} -> Pid ! {finished, Value, Ref};
                _ -> Pid ! {invalid_id, Ref}
            end,
            loop(State);
        {value, Ref, Value} ->
            loop(State#{Ref=>Value});
        {'EXIT', Worker, Reason} when Reason/=normal ->
            Ref = maps:fold(fun(Key, {W, _}, _Acc) when W =:= Worker -> Key;
                                (_, _, Acc) -> Acc
                             end, ok, State ),
            loop(State#{Ref=>error})
    end.

send_request(M, F, A) -> 
    ?SRV ! {request, Id = make_ref(), {M, F, A}},
    Id.

ask_result(Id) ->
    ?SRV ! {result, Id, self()},
    receive
        {in_progress, Id} -> not_ready_yet;
        {invalid_id, Id} -> not_avalilable_job;
        {finished, Value, Id} -> Value
        % error -> 
    end.

send_request(M, F, A, Node) -> 
    {js, Node} ! {request, Id = make_ref(), {M, F, A}},
    Id.


%This is an Erlang module that implements a job server. 
%A job server is a server that receives requests to perform some computation 
%and returns the result when the computation is done. The idea behind a job server 
%is to allow clients to offload expensive computations to a dedicated server, 
%freeing up their own resources to perform other tasks.

%The job server module has two interfaces: a server interface and a client interface. 

%The server interface consists of three functions: start/1, stop/0, and init/1. 
%The start/1 function starts the job server with a maximum number of workers, 
%which is passed as an argument. The stop/0 function stops the job server, 
%and the init/1 function initializes the job server and starts its main loop.

%The client interface consists of two functions: send_request/3 and ask_result/1. 
%The send_request/3 function sends a request to the job server to perform a computation. 
%It takes three arguments: the module, function, and arguments that the job server should use 
%to perform the computation. It returns a unique identifier for the request, 
%which the client can use later to ask for the result. The ask_result/1 function asks the job server for the result of a computation. 
%It takes a single argument, which is the unique identifier returned by the send_request/3 function. 
%If the computation is not done yet, ask_result/1 returns not_ready_yet. 
%If the computation is done, ask_result/1 returns the result.

% The loop/1 function is the main loop of the job server. It receives messages and processes them according to their type. 
% The loop maintains a state that keeps track of the requests and their status. The loop can receive several types of messages:
% stop: This message tells the job server to stop. When the loop receives this message, it prints a message to the console and terminates.
% {request, Ref, {M, F, A}}: This message tells the job server to perform a computation. 
% The message includes a reference to the request (Ref), the module (M), the function (F), and the arguments (A). 
% The loop spawns a worker process to perform the computation and stores the worker's process ID and the request status (started) in the state. 
% The loop then continues to process messages.
% %{result, Ref, Pid}: This message tells the job server that the result of a computation is available. 
% The message includes the reference to the request (Ref) and the process ID of the worker that performed the computation (Pid). 
% The loop looks up the status of the request in the state. If the request status is started, the loop sends a message to the worker ({in_progress, Ref}) 
% to let it know that the job is in progress. If the request status is finished, the loop sends the result of the computation to the client ({finished, Value, Ref}). 
% If the request status is anything else, the loop sends an error message to the client ({invalid_id, Ref}). The loop then continues to process messages.
% %{value, Ref, Value}: This message tells the job server that the result of a computation is available. 
% The message includes the reference to the request (Ref) and the result of the computation (Value). 
% The loop updates the status of the request to Value in the state and continues to process messages.
% %{'EXIT', Worker, Reason}: This message tells the job server that a worker process has terminated unexpectedly. 
% The message includes the process ID of the worker (Worker) and the reason for termination (Reason). The loop looks up the reference