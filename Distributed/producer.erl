-module(producer).
-compile(export_all).

-define(BUFF, {buffer, 'buff@ayman-ZenBook-UX363JA-UX363JA'}).

buffer(Queue) ->
    receive
        {produced, Item} ->
            NewQueue = queue:in(Item, Queue),
            buffer(NewQueue);
        {consume, Pid} ->
            case queue:out(Queue) of
                {NewQueue, Item} ->
                    Pid ! {item, Item},
                    buffer(NewQueue);
                [empty] ->
                    Pid ! empty_buffer,
                    buffer(Queue)
            end
        after 1000 ->
            end_of_input
    end.

start_buffer() ->
    buffer(queue:new()).

do_consume(BufferPid, RemainingRetries, MaxRetries, F) ->
    BufferPid ! {consume, self()},
    receive
        {item, Item} ->
            F(Item),
            do_consume(BufferPid, MaxRetries, MaxRetries, F);
        empty_buffer ->
            case RemainingRetries of
                0 ->
                    ok;
                _ ->
                    timer:sleep(2000),
                    do_consume(BufferPid, RemainingRetries - 1, MaxRetries, F)
            end
        after 2000 ->
            io:format("consumer down~n"),
            buffer_down
    end.

consume(BufferPid, MaxRetries, F) ->
    do_consume(BufferPid, MaxRetries, MaxRetries, F).

do_search(_BufferPid, leaf) ->
    ok;
do_search(BufferPid, {N, Left, Right}) ->
    case N >= 5 of
        true ->
            BufferPid ! {produced, N},
            ok;
        false ->
            ok
    end,
    do_search(BufferPid, Left),
    do_search(BufferPid, Right).

search(BufferPid, Tree) ->
    LeftPid = spawn(?MODULE, do_search, [BufferPid, element(2, Tree)]),
    RightPid = spawn(?MODULE, do_search, [BufferPid, element(3, Tree)]),
    Root = element(1, Tree),
    case Root >= 5 of
        true ->
            BufferPid ! {produced, Root},
            ok;
        false ->
            ok
    end,
    LeftPid ! self(),
    RightPid ! self().

main(Tree) ->
    BufferPid = spawn(?MODULE, start_buffer, []),
    spawn(?MODULE, search, [BufferPid, Tree]),
    spawn(?MODULE, consume, [BufferPid, 5, fun(Item) -> io:format("consumed ~p~n", [Item]) end]).
