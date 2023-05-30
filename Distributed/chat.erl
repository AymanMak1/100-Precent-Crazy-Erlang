-module(chat).
%% Server interface
-export([start/1, stop/0]).
-export([init/1, loop/1]).
%%Clenat interface
-export([login/1,logout/0, send/1]).

-define(CHAT, {chatsrv, 'server@ayman-ZenBook-UX363JA-UX363JA'}).
-define(MAX, 15).
%-define(ADD(A,B), A+B). %% ?ADD(2,3) --> 2+3

login(Nick) ->
    ?CHAT ! {log_in, self(), Nick},
    receive
        A -> A
    after
        5000 -> deny
    end.

-spec logout() -> {'log_out', pid()}.
logout() ->
    ?CHAT ! {log_out, self()}.

send(Msg) ->
    ?CHAT ! {send, self(), Msg}.

start(Args) ->
    register(chatsrv, spawn_link(?MODULE, init, [Args])). %% spawn_link(chat, init, [Args])

stop() ->
    ?CHAT ! stop. 

init(Max)->
    process_flag(trap_exit, true),
    InitState = {#{}, 0, Max},
    loop(InitState).

loop(State={Clients, Num, Max})->
    receive
        stop ->
            io:format("Server is terminating...~n");
        {log_in, Pid, Nick} when Num < Max -> %when size(Clients) < Max -> 
            link(Pid),
            io:format("~p is connecting to the conversation~n", [Pid]),
            Pid ! logged_in,
            loop({Clients#{Pid=>Nick}, Num+1, Max});
        {log_in, Pid, _Nick} ->
            Pid ! deny,
            loop(State);
        {log_out, Pid} -> 
            io:format("~p is leaving the conversation~n", [Pid]),
            loop({maps:remove(Pid, Clients), Num-1, Max});
        {'EXIT', Pid, Reason} when Reason /= normal ->
            io:format("~p is leaving the conversation~n", [Pid]),
            loop({maps:remove(Pid, Clients), Num-1, Max});
        {send, Pid, Msg} -> 
            #{Pid:=Nick} = Clients,
            NewMsg = Nick ++ ": " ++ Msg,
            maps:foreach(fun(ClPid, _) -> 
                            ClPid ! {msg, NewMsg}
                        end, Clients),
            loop(State);
        dump ->
            io:format("Server state:~p~n", [State]),
            loop(State);
        Other ->
            io:format("Unwanted message arrived:~p~n", [Other]),
            loop(State)
    end.
