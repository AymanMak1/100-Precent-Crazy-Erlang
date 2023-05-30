-module(vending).
-compile(export_all).
-define(SRV, {vending_machine, 'vm@ayman-ZenBook-UX363JA-UX363JA'}).

start(Price) ->
    register(vending_machine, spawn_link(?MODULE, init, [Price])),
    ok.

stop() ->
    ?SRV ! stop.

init(Price) -> 
    InitState = {[], Price},
    loop(InitState).

refill_machine(ListOfBeverages) ->
    ?SRV ! {refill_machine,lists:flatmap(
        fun({Beverage, Amount}) ->
            lists:duplicate(Amount, Beverage)
        end,
        ListOfBeverages
    )}, ok.
item_list() ->
     [{coke, 2}, {fanta, 3}, {sprite, 1}, {tonic, 1}, {water, 3}].

change_price(Price) ->
    ?SRV ! {change_price, Price}.

print_content() ->
    ?SRV ! print_content,
    ok.

print_price() ->
    ?SRV ! print_price,
    ok.

loop(State={Beverages,Price}) ->
    io:format("Machine : The machine is ready to accept orders!~n"),
    receive
       {stop} -> ok;
       {change_price, NewPrice} ->
            loop({Beverages,NewPrice});
       print_content ->
            io:format("Machine : Available beverages: ~p~n", [Beverages]),
            loop(State);
        print_price ->
            io:format("Machine : The price is set to: ~p~n", [Price]),
            loop(State);
        {refill_machine, NewListOfBeverages} ->
            io:format("Adding elements to the machine: ~p~n", [NewListOfBeverages]),
            loop({Beverages ++ NewListOfBeverages,Price});
        {insert_coins, Amount, Pid}  ->
            %io:format("Coins have been insert: ~p~n", [Amount]),
            case Amount < Price of
                true -> Pid ! insufficient_founds, loop(State);
                false -> 
                    io:format("Machine : Accepting order only from ~p~n", [Pid]),
                    Pid ! {sufficient_founds, lists:usort(Beverages)},
                    receive
                        cancelled ->
                            Pid ! {cancellation, Amount};
                        {select_beverage, Beverage, Pid} ->
                            case lists:member(Beverage, Beverages) of
                                true -> 
                                    Pid ! {transaction_succeeded, Beverage, Amount - Price},
                                    loop(State);
                                false ->
                                    Pid ! {beverage_not_found, Beverage},
                                    loop(State)
                            end
                            after 500 ->  
                                io:format("Ordering timed out!~n"),
                                loop(State)
                    end
            end
    end.


%% Client Inteface %%

insert_coins(Amount)->
    ?SRV ! {insert_coins, Amount, self()},
    receive
        insufficient_founds -> io:format("not enough money~n");
        {sufficient_founds, ListOfBeverages} -> 
            io:format("Select an item from the list (waiting time 10 sec): ~p~n", [ListOfBeverages]),
            ListOfBeverages
    end.

select_beverage(Order)->
    ?SRV ! {select_beverage, Order, self()},
    receive 
        {transaction_succeeded, Beverage, Return}-> 
            io:format("Ordering ~p was successful. Money returned: ~p~n", [Beverage, Return]);
        {beverage_not_found, Beverage} -> 
            io:format("the requested ~p beverage is not found.~n", [Beverage])
    end.

cancel()->
    ?SRV ! cancelled,
    receive
        {cancellation, Return} ->
            io:format("Money returned: ~p~n", [Return]),
            cancelled
    end.

%% Queuing for the vending machine %%