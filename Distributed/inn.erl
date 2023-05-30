-module(inn).
-compile(export_all).

start() ->
    net_kernel:start([inn_node, shortnames]),
    register(goblin_sup, spawn(goblin_sup, init, [])),
    spawn(inn_adventure, create_goblins, [3]),
    timer:sleep(200),
    spawn(inn_adventure, traveler).

create_goblins(N) ->
    [spawn(goblin_sup, goblin, [Id]) || Id <- lists:seq(1, N)],
    io:format("Created ~B goblin(s)~n", [N]).