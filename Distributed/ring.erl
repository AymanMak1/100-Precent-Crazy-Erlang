-module(ring).
-export([pipe/1, start/0]).

pipe(NextProcess) ->
    receive
        {forward, N} ->
            NextProcess ! {forward, N + 1},
            pipe(NextProcess);
        quit ->
            NextProcess ! quit
    end.

start() ->
    P = self(),
    A = spawn(ring, pipe, [P]),
    B = spawn(ring, pipe, [A]),
    C = spawn(ring, pipe, [B]),
    D = spawn(ring, pipe, [C]),
    E = spawn(ring, pipe, [D]),
    A ! {forward, 0},
    fun(quit) -> A ! quit, receive Msg -> Msg end;
        (Arg) -> E ! {forward, Arg}, receive Msg -> Msg end
    end.
