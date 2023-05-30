-module(client).

-export([start/1]).

start(Nick) ->
    case chat:login(Nick) of
        logged_in ->
            ClientPid = self(),
            spawn(fun() -> read(ClientPid) end),
            clientloop();
        deny -> 
            "Connection to the server failed";
        nick_eror ->
            "Nick name has to an atom"
    end.

clientloop() ->
    receive
        {msg, Msg} ->
            io:format("~p~n", [Msg]),
            clientloop();
        {text, Msg} ->
            chat:send(Msg),
            clientloop();
        stop ->
            chat:logout()
    end.

read(Pid) ->
    %flush(),
    case lists:droplast(io:get_line("--> ")) of
        "exit" ->
            %chat:logout();
            Pid ! stop;
        Msg ->
            %flush(),
            %chat:send(Msg),
            %flush(),
            Pid ! {text, Msg},
            read(Pid)
    end.

flush() ->
    receive
        {msg, A} -> 
            io:format("~p~n", [A]),
            flush()
    after 0 ->
        ok
    end.