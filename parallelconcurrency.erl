-module(parallelconcurrency).
-compile(export_all).

%%% Merge Sort %%%
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-2) + fib(N-1).

run()->
    Main = self(),
    register(divide, spawn( fun()-> divide_helper() end)),
    register(merge, spawn(fun()->merge_helper(Main) end)),
    register(first, spawn( fun()->first_worker() end ) ),
    register(second, spawn( fun()->second_worker() end)).
  

divide_helper() ->
    receive
       {sort, Divide, Merge, Sort, InputList} ->  
                {List1,List2} = Divide(InputList) , 
                first ! {fromDivide,Sort,List1}, 
                second ! {fromDivide,Sort ,List2} , 
                merge ! {fromDivide, Merge} , 
                divide_helper();

       stop -> merge ! stop, 
               first ! stop , 
               second ! stop , 
               io:format("Divide: Bye ");

       from_sub ->  io:format("Bye")
    end.

merge_helper(Main)->
    receive
     stop -> io:format(" Merge : Bye ~n");

        {fromDivide, Merge} -> Merge,
        receive
         stop -> io:format("Bye");
            {fromFirstWorker, L1} -> L1,
            receive
                {fromSecondWorker, L2} -> merge ! {  Merge(L1,L2) , Main }, merge_helper(Main);
                stop -> io:format("Bye") 
            end
        end;
        {Result,Pid} -> Pid ! Result, merge_helper(Main)
    end.
 

first_worker() ->
    receive 
    {fromDivide,SortFunction, List} -> merge ! {fromFirstWorker, SortFunction(List)}, first_worker();
    stop->io:format(" Worker : Bye ~n")

    end.

second_worker()->
 receive 
    {fromDivide,SortFunction, List} -> merge ! {fromSecondWorker, SortFunction(List)}, second_worker();
    stop->io:format("Worker : Bye ~n")
    end.


apply_alternately(_, _, []) -> [];
apply_alternately(F, G, L) ->
    MainPid = self(),
    ZippedList = lists:zip(lists:seq(1, length(L)), L),
    [spawn(fun() -> MainPid ! {self(), F(El), Idx} end) || {Idx, El} <- ZippedList, Idx rem 2 == 1] ++
    [spawn(fun() -> MainPid ! {self(), G(El), Idx} end) || {Idx, El} <- ZippedList, Idx rem 2 == 0],
    [receive {Pid, Value, Idx} -> {Pid,Value}  end || Idx <- lists:seq(1, length(L))].

%parallelconcurrency:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, [1,2,3,4,5]) == [2,4,4,8,6].
%parallelconcurrency:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, []) == [].
%parallelconcurrency:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, [1,22]) == [2,44].
%parallelconcurrency:apply_alternately(fun parallelconcurrency:fib/1, fun(E) -> E*2 end, [35, 2]) == [14930352,4].
%parallelconcurrency:apply_alternately(fun(E) -> E*2 end, fun parallelconcurrency:fib/1, [35, 35, 2]) == [70,14930352,4].
%parallelconcurrency:apply_alternately(fun(E) -> E*2 end, fun parallelconcurrency:fib/1, [35, 38, 2]) == [70,63245986,4].
%parallelconcurrency:apply_alternately(fun parallelconcurrency:fib/1, fun(E) -> E*2 end, [38, 2, 39]) == [63245986,4,102334155].
%parallelconcurrency:apply_alternately(fun(E) -> E + 1 end, fun erlang:atom_to_list/1, [1,apple]) == [2,"apple"].
%parallelconcurrency:apply_alternately(fun(E) -> E + 1 end, fun erlang:atom_to_list/1, [1, apple, 3, pear, 4, plum]) == [2,"apple",4, "pear",5, "plum"].
%parallelconcurrency:apply_alternately(fun parallelconcurrency:fib/1, fun erlang:atom_to_list/1, [1, apple, 3, pear, 4, plum]) == [1, "apple",3, "pear", 5, "plum"].
%parallelconcurrency:apply_alternately(fun parallelconcurrency:fib/1, fun erlang:atom_to_list/1, [1, apple, 33, pear, 40, plum]) == [1, "apple",5702887, "pear",165580141, "plum"].
%parallelconcurrency:apply_alternately(fun parallelconcurrency:fib/1, fun erlang:atom_to_list/1, [40, apple, 33, pear, 3, plum]) == [165580141, "apple",5702887, "pear",3, "plum"].

apply_alternately2(F, G, L) ->
    Main = self(),
    NewL = lists:zip(lists:seq(1, length(L)),L),
    register(fun1, spawn(fun() -> compute(F, Main) end)),
    register(fun2, spawn(fun() -> compute(G, Main) end)),
    [fun1 ! {I, E} || {I, E} <- NewL, I rem 2 == 1],
    [fun2 ! {I, E} || {I, E} <- NewL, I rem 2 == 0],
    Result = [
    receive
            {res, I, A} -> A
    end || {I, _E} <- NewL],
    fun1 ! kill,
    fun2 ! kill,
    Result.


compute(F, Main) -> 
    receive
        {I,E} -> Main ! {res, I, F(E)},
        compute(F, Main);
        kill -> killed
end.

applyAllPar(_, []) -> [];
applyAllPar([], _) -> [];
applyAllPar(FS, LS) ->
    MainPid = self(),
    Pids = [spawn(fun() -> MainPid ! {self(), F(E)} end) || F <- FS, E <- LS],
    [receive {Pid, Value} -> Value end || Pid <- Pids].

%parallelconcurrency:applyAllPar([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].
%parallelconcurrency:applyAllPar([fun(A) -> A+2 end], []) == [].
%parallelconcurrency:applyAllPar([], [apple, pear]) == [].
%parallelconcurrency:applyAllPar([fun erlang:is_list/1], [apple, pear]) == [false,false].
%parallelconcurrency:applyAllPar([fun erlang:is_list/1], [apple, pear, []]) == [false,false,true].

zip([],[]) -> [];
zip([],_) -> [];
zip(_,[]) -> [];
zip([H1|T1],[H2|T2]) -> [{H1,H2}| zip(T1,T2)].

apply_fun(F, Arg) ->
    try
        F(Arg)
    catch
        error:_ -> ok
    end.    

speculativeEval([],[]) -> no_proper_result;
speculativeEval([],_) -> no_proper_result;
speculativeEval(_,[]) -> no_proper_result;
speculativeEval(FS,LS) ->
    Main = self(),
    Pids = [spawn(fun() -> Main ! apply_fun(F,El) end) || {F,El} <- zip(FS,LS)],
    Result = receive Value when is_number(Value) -> Value end,
    [receive A -> A end || _ <- lists:seq(1, length(Pids) - 1)] , Result.

%parallelconcurrency:speculativeEval([], []) == no_proper_result.
%parallelconcurrency:speculativeEval([fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1,fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1], []) == no_proper_result.
%parallelconcurrency:speculativeEval([], [10,20,21,22]) == no_proper_result.
%parallelconcurrency:speculativeEval([fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1,fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1], [10,20,21,22]) == 55. %% very probably
%parallelconcurrency:speculativeEval([fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1,fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1], [20,10,21,22]) == 55. %% very probably
%parallelconcurrency:speculativeEval([fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1,fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1], [22,24,20,10,21,22]) == 55.
%parallelconcurrency:speculativeEval([fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1,fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1], [apple, pear, 22, plum, foo, bar]) == 17711.
%parallelconcurrency:speculativeEval([fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1,fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1], [apple, pear, 22, plum, foo, bar]) == 17711.
%parallelconcurrency:speculativeEval([fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1,fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1], [apple, pear, 22, plum, 23, bar]) == 17711.
%parallelconcurrency:speculativeEval([fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1,fun parallelconcurrency:fib/1, fun parallelconcurrency:fib/1], [apple, pear, plum, orange, foo, bar]) == no_proper_result.
sort(L) ->
    case length(L) of
        0 -> [];
        1 -> L;
        _ ->
            {Min, Rest} = find_min(L),
            [Min | sort(Rest)]
    end.

find_min([H|T]) ->
    find_min(T, H, []).
find_min([], Min, Rest) ->
    {Min, Rest};
find_min([H|T], Min, Rest) when H < Min ->
    find_min(T, H, [Min|Rest]);
find_min([H|T], Min, Rest) ->
    find_min(T, Min, [H|Rest]).

merge_sort([])-> [];
merge_sort(L)->
    MainPid = self(),
    {L1,L2} =  lists:split(length(L) div 2, L),
    spawn(fun()-> MainPid ! {self(),sort(L1)} end),
    spawn(fun()-> MainPid ! {self(),sort(L2)} end),
    receive
        {_Pid1,Val1}-> 
            receive
                {_Pid2,Val2} -> lists:merge(Val1,Val2)
            end
    end.


merge_sort2([]) -> [];
merge_sort2(L) when length(L) == 1 -> L;
merge_sort2(L) ->
    Main = self(),
    {L1, L2} = lists:split(length(L) div 2, L),
    spawn(fun() ->  Main ! merge_sort2(L1) end),
    spawn(fun() ->  Main ! merge_sort2(L2) end),
    receive
        Val1 -> Val1 
    end,
    receive
        Val2 -> Val2
    end,
    lists:merge(Val1,Val2).

%parallelconcurrency:merge_sort([111,11,1,12,13,23,2,3,31,22,253,4,221]) == [1,2,3,4,11,12,13,22,23,31,111,221,253]
%parallelconcurrency:merge_sort([5,4,3]) == [3,4,5]
%parallelconcurrency:merge_sort([5,4,3,5,1,3,2]) == [1,2,3,3,4,5,5]
%parallelconcurrency:merge_sort([5,4,3,1,2]) == [1,2,3,4,5]
%parallelconcurrency:merge_sort([5,4,3,1,2, 0, -3, -211]) == [-211,-3,0,1,2,3,4,5]

pmfm(F, G, H, L) ->
    Main = self(),
    PidH = spawn(fun() -> process_h(H, Main) end),
    PidG = spawn(fun() -> process_g(G, PidH) end),
    PidF = spawn(fun() -> process_f(F, PidG) end),
    Pids = [PidF ! {self(), E} || E<-L],
    [receive
        {result, R} -> R
    end || _ <- Pids].

applyFun(F,D)->
    try
        F(D)
    catch
        _:_ -> "failed"
    end.
process_f(F, PidG) ->
    receive
        %stop -> PidG ! stop;
        {_Pid, D} -> PidG ! {self(), applyFun(F,D)}, process_f(F,PidG)
    end.

process_g(G, PidH) ->
    receive
        %stop -> PidG ! stop;
        {_Pid, D} -> PidH ! {self(), applyFun(G,D)}, process_g(G,PidH)
    end.

process_h(H, Main) ->
    receive
        %stop -> PidG ! stop;
        {_Pid, D} -> Main ! {self(), applyFun(H,D)}, process_g(H,Main)
    end.

%parallelconcurrency:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:atom_to_list/1, []) ==[]
%parallelconcurrency:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:atom_to_list/1, [1, apple, 2])==["apple"]
%parallelconcurrency:pmfm(fun(X)-> X*2 end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [1, 2, 3, 4, 5, 6]) ==[1, 2, 3, 4, 5, 6]
%parallelconcurrency:pmfm(fun(X)-> X end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [11, 12, 13, 14, 15, 16]) == [6,7,8]

zip2([],_)-> [];
zip2(_,[])-> [];
zip2([H1|T1],[H2|T2])-> [{H1,H2} | zip2(T1,T2)].

multi(F,L1,L2)->
    Main = self(),
    Pids = [spawn(fun()-> Main ! applyMultiFun(F,E1,E2,Main) end) || {E1,E2} <- zip2(L1,L2)],
    [receive 
        error -> {'Exit', "Non matching types"};
        Val -> Val
        end 
    || _ <- Pids].

applyMultiFun(F,E1,E2,Main)->
    try
        F(E1,E2)
    catch
        error:badarith -> Main ! error
    end.

%parallelconcurrency:multi(fun erlang:'+'/2, [], []) == []
%parallelconcurrency:multi(fun erlang:'+'/2, [1,2], [3,4]) == [4,6]
%parallelconcurrency:multi(fun erlang:'+'/2, [1,2,5], [3,4]) == [4,6]
%parallelconcurrency:multi(fun erlang:'+'/2, [1,2,5], [3,4, 8, 9]) == [4,6,13]
%parallelconcurrency:multi(fun(X, Y) -> {X, Y, math:pow(X, Y)} end, [1,2,5], [3,4, 8, 9]) == [{1,3,1.0},{2,4,16.0},{5,8,390625.0}]
%parallelconcurrency:multi(fun erlang:'+'/2, [1], [apple]) == {'EXIT',"Non matching types"}
%bucket_sort(L)-> bucket_sort(L,[],[])
bucket_sort(L) ->
    case L of
        [] -> [];
        [_] -> L;
        _ ->
            Mean = lists:sum(L) div length(L),
            {Bucket1, Bucket2} = divide(L, Mean),
            Pid1 = spawn(fun() -> bucket_sort(Bucket1) end),
            Pid2 = spawn(fun() -> bucket_sort(Bucket2) end),
            SortedBucket1 = receive
                {Pid1, Result} -> Result
            end,
            SortedBucket2 = receive
                {Pid2, Result} -> Result
            end,
            merge(SortedBucket1, SortedBucket2)
    end.

divide(L, Mean) ->
    divide(L, Mean, [], []).

divide([], _, Bucket1, Bucket2) ->
    {Bucket1, Bucket2};
divide([X|L], Mean, Bucket1, Bucket2) when X < Mean ->
    divide(L, Mean, [X|Bucket1], Bucket2);
divide([X|L], Mean, Bucket1, Bucket2) ->
    divide(L, Mean, Bucket1, [X|Bucket2]).

merge([], L) -> L;
merge(L, []) -> L;
merge([X|L1], [Y|L2]) when X < Y ->
    [X | merge(L1, [Y|L2])];
merge([X|L1], [Y|L2]) ->
    [Y | merge([X|L1], L2)].

%parallelconcurrency:bucket_sort([111,11,1,12,13,23,2,3,31,22,253,4,221]) == [1,2,3,4,11,12,13,22,23,31,111,221,253]
%parallelconcurrency:bucket_sort([5,4,3]) == [3,4,5]
%parallelconcurrency:bucket_sort([5,4,3, 1,2]) == [1,2,3,4,5]
%parallelconcurrency:bucket_sort([5,4,3, 1,2, 0, -3, -211]) == [-211,-3,0,1,2,3,4,5]

pany(F,L)->
    MainPid = self(),
    Pids = [spawn(fun()-> MainPid ! {F(E), E} end) || E <- L],
    Res = [receive 
        {true, Elem} -> {true, Elem};
        {false, _} -> false
    end || _  <- Pids],
    checkIfTrue(Res).
checkIfTrue([])-> false;
checkIfTrue([H|T])->
    case H of
        {Bool,Val} -> {Bool,Val};
        false -> checkIfTrue(T)
    end.
%parallelconcurrency:pany(fun(X) -> X > 6 end, [1,2,3]) == false
%parallelconcurrency:pany(fun(X) -> X > 6 end, [11,12,13]) == {true, 11} or {true, 12} or {true, 13}
%parallelconcurrency:pany(fun erlang:is_atom/1, [1,apple,2]) == {true, apple}

mapReduce(M, Text) ->
    % Start the reduce process
    ReducePid = spawn(fun() -> reduce(self(), []) end),

    % Start all the map processes
    MapPids = [spawn(fun() -> map(ReducePid) end) || _ <- lists:seq(1, M)],

    % Split the text into lines
    Lines = string:tokens(Text, "\n"),

    % Randomly send each line to a map process
    lists:foreach(fun(Line) ->
        RandomMapPid = lists:nth(random:uniform(M), MapPids),
        RandomMapPid ! {self(), Line}
    end, Lines),

    % Wait for final result from the reduce process
    receive
        {ReducePid, Result} ->
            % Kill all the workers and the reduce
            lists:foreach(fun(Pid) -> Pid ! stop end, [ReducePid|MapPids]),
            % Return the collected result
            Result
    end.

map(ReducePid) ->
    receive
        {Pid, Line} ->
            % Split the line into words and send each word as a tuple to the reduce process
            Words = string:tokens(Line, " "),
            lists:foreach(fun(Word) -> ReducePid ! {Word, 1} end, Words),
            % Wait for the next input
            map(ReducePid)
    end.

reduce(MapReducePid, CurrentResult) ->
    receive
        {Word, 1} ->
            % Check if the word is already in the result list
            case lists:keyfind(Word, 1, CurrentResult) of
                false ->
                    % If the word is not in the result list, append it
                    NewResult = CurrentResult ++ [{Word, 1}];
                {_, Count} ->
                    % If the word is in the result list, update the count
                    NewResult = lists:keyreplace(Word, 1, CurrentResult, {Word, Count + 1})
            end,
            % Wait for the next input
            reduce(MapReducePid, NewResult);
        stop ->
            % When no more messages arrive, print "I am done!" and send the result to mapReduce
            io:format("I am done!~n"),
            MapReducePid ! {self(), CurrentResult};
        _ ->
            % Ignore any other messages
            reduce(MapReducePid, CurrentResult)
    after
        % Wait for 2 seconds of inactivity before assuming that there are no more messages
        2000 ->
            % Print "I am done!" and send the result to mapReduce
            io:format("I am done!~n"),
            MapReducePid ! {self(), CurrentResult}
    end.

iterate(F,L,N) ->
    Last = lists:foldr(fun(_,Acc)-> 
                                spawn(parallelconcurrency,worker_iterate,[F,Acc]) end,
                                self(),
                                lists:seq(1,N)),
    [Last ! E || E <- L],
    [receive Res -> Res end || _ <- L].


worker_iterate(F,Next)->
        receive
            Data -> Next ! F(Data), worker_iterate(F,Next)                                    
        end.

%iterate(F,[],N)-> [];
%iterate(F,L,0)-> L;
%iterate(F,[H|T],N) when N /= 0 ->
%    MainPid = self(),
%    Pids = [spawn(fun()-> MainPid ! applyFuncNtimes(F,H,N) end ) | iterate(F,T,N)],
%    [receive Val -> Val  end|| _ <- Pids].
%
%applyFuncNtimes(F,E,Count) when Count /= 0->
%    try
%        applyFuncNtimes(F,F(E),Count-1)
%    catch
%        _:_ ->  "Failing operation"
%    end;
%applyFuncNtimes(F,E,Count)->
%    E.

%parallelconcurrency:iterate(fun(X) -> X + 1 end, [5,8,23], 11) == [16,19,34].
%parallelconcurrency:iterate(fun(X) -> X + 1 end, lists:seq(1,10), 15) == [16,17,18,19,20,21,22,23,24,25].
%parallelconcurrency:iterate(fun(X) -> X + 1 end, lists:seq(1,20), 15) == [16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35].
%parallelconcurrency:iterate(fun(X) -> X + 1 end, lists:seq(1,20), 1) == [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21].
%parallelconcurrency:iterate(fun(X) -> X ++ "0" end, ["a", "b", "c"], 1) == ["a0","b0","c0"].
%parallelconcurrency:iterate(fun(X) -> X ++ "0" end, ["a", "b", "c"], 5) ==["a00000","b00000","c00000"].
%parallelconcurrency:iterate(fun(X) -> X ++ "0" end, [], 5) == [].
%parallelconcurrency:iterate(fun(X) -> X ++ "0" end, ["a", "b", "c"], 0) == ["a","b","c"].
%:iterate(fun(X) -> X + 0 end, [2,a,3], 1) == "Failing operation"


pmfm3(F, G, H, L) ->
    Main = self(),
    register(f_worker, spawn(fun() -> f_helper(F) end)),
    register(g_worker, spawn(fun() -> g_helper(G) end)),
    register(h_worker, spawn(fun() -> h_helper(H,Main) end)),
    [spawn(fun() -> f_worker ! {from_main,E} end) || E <- L],
    ReceivedList =[receive
        {from_H,Value}->Value
     end || _<-L],
     Final = lists:filter(fun(Elem)-> Elem /= "false" end, ReceivedList),
     f_worker ! stop,
     g_worker ! stop,
     h_worker ! stop,

     Final.

f_helper(F) ->
    receive
        {from_main,Value} -> FValue = F(Value),
            g_worker ! {from_F, FValue},f_helper(F);
        stop -> stopped
    end.

g_helper(G) ->
    receive
        {from_F, FValue} ->
            case G(FValue) of
                true -> h_worker ! {from_G, FValue},g_helper(G);
                false -> h_worker ! {from_G,false},g_helper(G)
            end;
            stop -> stopped

    end.
h_helper(H, Main) ->
    receive
        {from_G, false} -> Main ! {from_H, "false"},h_helper(H,Main);
        {from_G, FValue} -> Main ! {from_H, H(FValue)},h_helper(H,Main);
        stop -> stopped
    end.

%parallelconcurrency:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:atom_to_list/1, []) ==[]
%parallelconcurrency:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:atom_to_list/1, [1, apple, 2])==["apple"]
%parallelconcurrency:pmfm(fun(X)-> X*2 end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [1, 2, 3, 4, 5, 6]) ==[1, 2, 3, 4, 5, 6]
%parallelconcurrency:pmfm(fun(X)-> X end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [11, 12, 13, 14, 15, 16]) == [6,7,8]
%parallelconcurrency:pmfm(fun(X)-> X*2 end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [1, apple, 6]).
%"Failing operation"
%parallelconcurrency:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:list_to_atom/1, [1, apple, 2]).
%"Failing operation"

player(NextPid) ->
    receive
        Msg when Msg rem 3 == 0 ->
            NextPid ! Msg;
        Msg ->
            NewMsg = Msg * 10 + (3 - (Msg rem 3)),
            NextPid ! NewMsg
    end.
start(InitMsg) ->
    Pid5 = spawn_link(parallelconcurrency, player, [[]]),
    Pid4 = spawn_link(parallelconcurrency, player, [Pid5]),
    Pid3 = spawn_link(parallelconcurrency, player, [Pid4]),
    Pid2 = spawn_link(parallelconcurrency, player, [Pid3]),
    Pid1 = spawn_link(parallelconcurrency, player, [Pid2]),
    Pid1 ! InitMsg,
    receive
        FinalMsg -> FinalMsg
    end.


% parallelconcurrency:start(1) == 12333


finn(Pid)->
    io:format("Finn: What time is it?~n"),
    Pid ! {what_time_is_it, self()},
    receive
        adventure_time -> io:format("Finn: That's right buddy~n")
    end.

jake() ->
    receive
        {what_time_is_it, FinnPid} -> io:format("Jake: Adventure time!~n"), FinnPid ! adventure_time
    end.

begin_adventure()->
    JakePid = spawn(fun()-> jake() end),
    _FinnPid = spawn(fun()-> finn(JakePid) end).

%parallelconcurrency:begin_adventure().