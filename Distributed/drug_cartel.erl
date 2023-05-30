-module(drug_cartel).
-compile(export_all).

%%%%% Drug cartel %%%%%
-define(CARTEL, {guard, 'cartel@ayman-ZenBook-UX363JA-UX363JA'}).
warehouse()->
    CorrectPassword = "WeGotEm",
    register(guard, spawn_link(?MODULE, guard_loop, [[], CorrectPassword])),
    WrongPassword = "ComeOnDawg",
    [spawn(fun()-> bad_guy(CorrectPassword) end) || _ <- lists:seq(1,3)],
    [spawn(fun()-> bad_guy(WrongPassword) end) || _ <- lists:seq(1,3)],
    timer:sleep(1000),    
    fbi().

guard(Processes, Pass) ->
    guard_loop(Processes, Pass).

guard_loop(TraffickersPids, Pass)->
    receive
        {let_in, Who} -> Who ! whats_the_password,
                        guard(TraffickersPids,Pass) ;
        {password,Password,Who}->
            case Password == Pass of
                true ->
                    Who ! come_in,
                    guard([Who | TraffickersPids],Pass);
                false ->
                    Who ! go_away, 
                    guard(TraffickersPids,Pass)
            end;
        im_a_cop-> 
            io:format("Guard: Cops are here!~n"),
            [Trafficker ! cops_are_here || Trafficker <- TraffickersPids]
    end.

bad_guy(Password)->
    ?CARTEL ! {let_in, self()},
    receive
        whats_the_password -> 
            ?CARTEL ! {password,Password,self()},
            receive
                come_in ->
                    io:format("Trafficker In~n"),
                    receive
                        cops_are_here -> io:format("I'm outta here!") 
                    end;
                go_away -> io:format("Guard didn't let me in.~n")
            end
    end.

fbi()->
    ?CARTEL ! {let_in, self()},
    receive
        whats_the_password -> ?CARTEL ! im_a_cop
    end.