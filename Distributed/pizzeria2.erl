-module(pizzeria2).
-compile(export_all).

-define(PIZZ, {server,'pizz@ayman-ZenBook-UX363JA-UX363JA'}).

open() ->
    register(server,spawn_link(?MODULE, pizzeria, [[]])).

close() ->
    ?PIZZ ! close.

order(Pizza) ->
    ?PIZZ ! {order, self(), Pizza}.

where_is_my_pizza() ->
    ?PIZZ ! {what_takes_so_long, self()}.

cook(margherita) ->
    timer:sleep(500);
cook(calzone) ->
    timer:sleep(600).

pizzeria(Orders) ->
    receive
        {order, Client, Pizza} ->
            {_,Ref} = spawn_monitor(fun() -> cook(Pizza) end),
            NewOrder = {Ref, Client, Pizza},
            pizzeria([NewOrder | Orders]);
        {what_takes_so_long, Client} ->
            case lists:keyfind(Client, 2, Orders) of
                {_Ref, _, Pizza} ->
                    Client ! {cooking, Pizza};
                false ->
                    Client ! nothing_was_ordered
            end,
            pizzeria(Orders);
        {'DOWN', Ref, _, _, _} ->
            case lists:keyfind(Ref, 1, Orders) of
                {_Ref, Client, Pizza} ->
                    Client ! {delivered, Pizza},
                    NewOrders = lists:keydelete(Ref, 1, Orders),
                    pizzeria(NewOrders);
                false ->
                    io:format("it's not found~n"),
                    pizzeria(Orders)
            end;
        close ->
            ok
    end.