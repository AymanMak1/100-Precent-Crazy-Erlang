-module(whisper).
-compile(export_all).

player(Pid) ->
    receive
       Integer ->
            Digit = findDigit(Integer),
            Res = integer_to_list(Integer) ++ integer_to_list(Digit),
            Pid ! list_to_integer(Res)
    end.

findDigit(Integer)-> findDigit(Integer,1).
findDigit(Integer, Digit) ->
    case (Integer + Digit) rem 3 == 0 of
        true -> Digit;
        false -> findDigit(Integer, Digit + 1)
    end.

start(InitialMessage) ->
    P1 = spawn(whisper, player, [self()]),
    P2 = spawn(whisper, player, [P1]),
    P3 = spawn(whisper, player, [P2]),
    P4 = spawn(whisper, player, [P3]),
    P5 = spawn(whisper, player, [P4]),
    P5 ! InitialMessage,
    receive
        LastMessage -> LastMessage
    end.