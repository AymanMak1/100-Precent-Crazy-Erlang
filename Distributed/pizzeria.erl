-module(pizzeria).
-compile(export_all).

-define(PIZZ, {server,'pizz@ayman-ZenBook-UX363JA-UX363JA'}).

start(Menu)->
    register(server, spawn_link(?MODULE,init,[Menu])).

init(Pizzas)->
    IntialState = {Pizzas,#{}},
    loop(IntialState).

loop(State={Pizzas,OrdersIDs})->
    receive
        {order,Ref,Pid,PizzaName}->
            case maps:is_key(PizzaName, Pizzas) of
                true->
                    Pid ! {order_received,Ref},
                    Time = map_get(PizzaName, Pizzas),
                    spawn_link(fun()-> cook(Ref,Time) end),
                    loop({Pizzas,maps:put(Ref,cooking,OrdersIDs)});
                false->
                    Pid ! {order_received,Ref},
                    loop({Pizzas,maps:put(Ref,not_available,OrdersIDs)})
            end;
        {ready, Ref}->
            loop({Pizzas,maps:update(Ref,ready,OrdersIDs)});
        {is_it_ready,Pid,Ref}->
            case maps:get(Ref, OrdersIDs) of
                cooking -> Pid ! cooking;
                ready -> Pid !ready;
                not_available -> Pid ! not_available
            end,
            loop(State);
        {menu_please,Pid}->
            Pid ! {menu,Pizzas}
    end.

cook(OrderID, Time)->
    timer:sleep(Time * 1000),
    ?PIZZ ! {ready, OrderID}.

order(PizzaName)->
    Ref = make_ref(),
    ?PIZZ ! {order, Ref, self(),PizzaName},
    receive
        {order_received, Id}-> Id
    end.

ready(Ref)->
    ?PIZZ ! {is_it_ready, self(),Ref},
    receive
        Status -> Status    
    end.

menu()->
    ?PIZZ ! {menu_please, self()},
    receive
        {menu,Menu}-> Menu
    end.

waiting(N) ->
    MenuMap = menu(),
    MenuList = maps:to_list(MenuMap),
    RandomIndex = rand:uniform(length(MenuList)),
    {RandomPizza,_Time} = lists:nth(RandomIndex,MenuList),
    io:format("Ordering ~p...~n", [RandomPizza]),
    Ref = order(RandomPizza),
    io:format("Ref ~p...~n", [Ref]),
    timer:sleep(N * 1000),
    Status = ready(Ref),
    io:format("Status ~p...~n", [Status]),
    case Status of
        ready -> io:format("Eating... ~p~n", [RandomPizza]);
        cooking -> io:format("Slow... ~p~n", [RandomPizza])
    end.