-module(binary_tree).
-compile(export_all).


spawn_tree(N) when N > 0 ->
    spawn(fun() -> spawn_children(N) end);
spawn_tree(_) ->
    timer:sleep(2).

spawn_children(N) when N > 0 ->
    spawn(fun() -> spawn_tree(N div 2) end),
    spawn_children(N - 1);
spawn_children(_) ->
    ok.

start() ->
    spawn_tree(220).
