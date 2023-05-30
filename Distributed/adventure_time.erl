-module(adventure_time).
-compile(export_all).

finn(JakePid)->
    io:format("Finn: What time is it?~n"),
    JakePid ! {what_time_is_it, self()},
    receive
        adventure_time -> io:format("Finn: That's right buddy~n");
        _ -> finn(JakePid)
    end.

jake()->
    receive
        {what_time_is_it, FinnPid} -> io:format("Jake: Adventure time!~n"),
        FinnPid ! adventure_time
    end.

begin_adventure()->
    JakePid = spawn(fun()-> jake() end),
    spawn(fun()-> finn(JakePid) end).
