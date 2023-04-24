-module(full).
-compile(export_all).

differences([],_)->[];
differences(L1,[])->L1;
differences([H1|T1],[H2|T2]) when H1 /= H2->
    [H1] ++ differences(T1,T2);
differences([_H1|T1],[_H2|T2])->
    differences(T1,T2).
%full2:differences("","") == [].
%full2:differences("","apple") == [].
%full2:differences("apple", "") == "apple".
%full2:differences("apple", "apple") == [].
%full2:differences("apple", "peach") == "apple".
%full2:differences("apple", "apfel") == "ple".
%full2:differences([1,2,3], [3,2,1]) == [1,3].
applyAll([],_)-> [];
applyAll(Funcs,Lst)->
    [Func(El)|| Func <-Funcs, El <- Lst].

applyAll2([], _) -> [];
applyAll2(_, []) -> [];
applyAll2([Func|Funcs], Lst) ->
    [Func(El) || El <- Lst] ++ applyAll2(Funcs, Lst).

%full2:applyAll([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].
%full2:applyAll([fun(A) -> A+2 end], []) == [].
%full2:applyAll([], [apple, pear]) == [].
%full2:applyAll([fun erlang:is_list/1], [apple, pear]) == [false,false].
%full2:applyAll([fun erlang:is_list/1], [apple, pear, []]) == [false,false,true].

getPositions(_,[])->[];
getPositions(C,Lst) ->
    [Idx || {El,Idx}<-lists:zip(Lst,lists:seq(1,length(Lst))), C == El].


getPositions2(C,Lst)->getPositions2(C,Lst,1).
getPositions2(_,[],_)->[];
getPositions2(C,[H|T],Idx) when C /= H ->
    getPositions2(C,T,Idx+1);
getPositions2(C,[_H|T],Idx) ->
    [Idx | getPositions2(C,T,Idx+1)].
    
%full2:getPositions($e, "apple") == [5].
%full2:getPositions($p, "apple") == [2,3].
%full2:getPositions(1, []) == [].
%full2:getPositions(1, [1,3,2,1,2,34,21,1,1,4]) == [1,4,8,9].

merge([],[])-> [];
merge([],[T2])-> [T2];
merge([H1|T1],[H2|T2])->
    [H1]++[H2] ++ merge(T1,T2).
    
riffleShuffle([])-> [];
riffleShuffle(Lst) ->
    HalfLen= trunc(length(Lst)/2),
    L1 = lists:sublist(Lst, 1, HalfLen),
    L2 = lists:sublist(Lst, HalfLen+1, length(Lst)),
    merge(L1,L2).
%full2:riffleShuffle([]) == [].
%full2:riffleShuffle([1]) == [1].
%full2:riffleShuffle([1,2]) == [1,2].
%full2:riffleShuffle([1,2,3]) == [1,3,2].
%full2:riffleShuffle([1,2,3,4]) == [1,3,2,4].
%full2:riffleShuffle([1,2,3,4,5]) == [1,4,2,5,3].
%full2:riffleShuffle([1,2,3,4,5,6]) == [1,4,2,5,3,6].
%full2:riffleShuffle([1,2,3,4,5,6,7]) == [1,5,2,6,3,7,4].
%full2:riffleShuffle([1,4,2,5,3,6,7]) == [1,3,4,6,2,7,5].

applyFun(F, X) ->
  try
    [F(Elem) || Elem <- X]
  catch
    _:_ ->
      ["bad_fun_argument" || _ <- X]
  end.

applyAll3(Funs, Xs) ->
  [applyFun(F, Xs) || F <- Funs].
%full2:applyAll3([fun(A) -> A+2 end], [1,apple]) == [3,bad_fun_argument].
%full2:applyAll3([fun erlang:atom_to_list/1, fun(A) -> A*2 end], [1,apple,3, '12']) ==[bad_fun_argument, "apple", bad_fun_argument, "12", 2,  bad_fun_argument, 6, bad_fun_argument].
%full2:applyAll3([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].

repeat_while(Pred,Func,El)->
    case Pred(El) of
        true -> [El] ++ repeat_while(Pred,Func,Func(El));
        false -> []
    end.
%full2:repeat_while(fun(E) -> E > 10 end, fun(E) -> E - 1 end, 20) == [20,19,18,17,16,15,14,13,12,11]
%full2:repeat_while(fun(E) -> E > 65 end, fun(E) -> E - 1 end, 66) == "B"
%full2:repeat_while(fun(E) -> E > 65 end, fun(E) -> E - 1 end, 65) == []
%full2:repeat_while(fun(E) -> E > 600 end, fun(E) -> E - 1 end, 10) == []

freq(L) -> freq(L,#{}).
freq([],Freqs)->
    maps:to_list(Freqs);
freq([H|T],Freqs)->
    Incrementer = fun(Count) -> Count + 1 end,
    Values = maps:update_with(H, Incrementer, _Default=1, Freqs),
    freq(T,Values).

elems_repeated_at_least_ntimes(N,Lst) ->
    [El || {El,Freq} <- freq(Lst), Freq >= N].

%full2:elems_repeated_at_least_ntimes(0, [1,4,3]) == [1,3,4]
%full2:elems_repeated_at_least_ntimes(-3, [1,4,3]) == [1,3,4]
%full2:elems_repeated_at_least_ntimes(2, [1,4,3]) == []
%full2:elems_repeated_at_least_ntimes(2, [1,4,1,3]) == [1]
%full2:elems_repeated_at_least_ntimes(2, [2,1,4,1,3,2]) == [1,2]
%full2:elems_repeated_at_least_ntimes(2, [2,1,4,1,3,2,2]) == [1,2]
%full2:elems_repeated_at_least_ntimes(2, "Mississippi") == "ips"
%full2:elems_repeated_at_least_ntimes(4, "Mississippi") == "is"

eval_polynomial([],_)->0;
eval_polynomial(Coeffs,X)->
    lists:sum([Coeff*math:pow(X,Idx) || {Coeff,Idx} <- lists:zip(Coeffs,lists:reverse(lists:seq(0,length(Coeffs)-1)))]).

eval_polynomial2(Coeffs,X)->eval_polynomial2(Coeffs,X,length(Coeffs)).
eval_polynomial2(_Coeffs,_X,0)-> 0;
eval_polynomial2([Coeff|Coeffs],X,Idx)->
    Coeff*math:pow(X,Idx-1) + eval_polynomial2(Coeffs,X,Idx-1).
%full2:eval_polynomial([3,4,5,6,0,1], 1) == 19.0
%full2:eval_polynomial([3,4,5,6,0,1], 3) == 1243.0
%full2:eval_polynomial([32,4,5,1,0], 1) == 42.0
%full2:eval_polynomial([32,4,5,1,0], 0) == 0.0
%full2:eval_polynomial([], 3) == 0

until(Pred,Func,El)->
    case Pred(El) of
        false -> [Func(El)] ++ until(Pred,Func,Func(El));
        true -> []
    end.
%full:until(fun(X)-> X>10 end, fun(X)-> X+2 end,3) ==[5,7,9,11] 

twice(Func,Val)-> twice(Func,Val,2).
twice(_Func,Val,0) ->
    Val;
twice(Func,Val,Count) when Count /= 0 ->
    twice(Func,Func(Val),Count-1).
%full:twice(fun(X)-> X+2 end,3) == 7.

flip(Func)->
    fun (X, Y) -> Func(Y, X) end.

bin_to_decimal(Bins)->
    trunc(lists:sum([Bin * math:pow(2,Idx) || {Bin,Idx} <- lists:zip(Bins,lists:reverse(lists:seq(0,length(Bins)-1)))])).

bin_to_decimal2(L) -> bin_to_decimal2(L,length(L)).
bin_to_decimal2([],0)-> 0;
bin_to_decimal2([Bin|Bins],Count)->
    trunc(Bin * math:pow(2,Count-1) + bin_to_decimal2(Bins,Count-1)).

%full:bin_to_decimal([1,0,1,0,1,0])== 42.

divisors(E) -> [Divisor || Divisor <- lists:seq(1,E-1), E rem Divisor == 0].
first_n_abundant_nums(N)->first_n_abundant_nums(N,N).
first_n_abundant_nums(_N,0)->[];
first_n_abundant_nums(N,I)->
    Res = case lists:sum(divisors(I)) > I of
        true -> [I] ++ first_n_abundant_nums(N,I-1);
        false-> first_n_abundant_nums(N,I-1)
    end,
    lists:reverse(Res).

half(I)-> I div 2.
double(I)-> I*2.
is_even(I)-> I rem 2 == 0.

multiplier_h(L,R)->
    Res=[],
    case L > 1 of
        true-> Res ++ [{L,is_even(L),R}] ++ multiplier_h(half(L),double(R));
        false-> Res ++ [{L,is_even(L),R}]
    end.
multiplier(L,R)->
    lists:sum([DoubleRs || {_,false,DoubleRs} <- multiplier_h(L,R)]).
%full:multiplier(17,34) == 578
partition(Pred,Lst)->
    Satisfy = [El || El <- Lst, Pred(El)],
    NotSatisfy = [El || El <- Lst, not Pred(El)],
    [Satisfy] ++ [NotSatisfy].

partition2(_Pred, []) ->
    [[] , []];
partition2(Pred, [H | T]) ->
    [Satisfy, NotSatisfy] = partition2(Pred, T),
    case Pred(H) of
       true -> [[H | Satisfy], NotSatisfy];
       false -> [Satisfy, [H | NotSatisfy]]
    end.
%full2:partition(fun(X)-> X>10 end ,[7,10,8,155,133] )==[[155,133],[7,10,8]]
%full2:partition(fun(X)-> X rem 2==0  end ,[7,8,2222,223] )==[[8,2222],[7,223]]
%full2:partition(fun(X)-> is_atom(X)  end ,[[1,2],[alma],hello] )==[[hello],[[1,2],[alma]]]
figure({X,X,_Z}) -> {isosceles,not_rectangle};
figure({_X,Y,Y}) -> {isosceles,not_rectangle};
figure({X,_Y,X}) -> {isosceles,not_rectangle};
figure({X,_Y,X}) -> {isosceles,not_rectangle};
figure({X,X,X}) -> {equilateral,not_rectangle};
figure({_X,_Y,_Z}) -> "not_a_proper_triangle";
figure({X,X,X,X}) -> "square".
%figure({1,2,3})= "not_a_proper_triangle"
%figure({2,2,3})= {isosceles,not_rectangle} 
%figure({2,2,2,2})= square

min(X,Y) when X > Y -> Y;
min(X,_Y) -> X.

%upperLower(C) when C >= $a or C =< $z -> string:to_upper(C);
%upperLower(C) when C >= $A || C =< $Z -> string:to_lower(C);
%upperLower(C) -> C.
upperLower(Str) ->
    [ upperLowerChar(C) || C <- Str ].

upperLowerChar(C) ->
    case lists:member(C, lists:seq($a, $z)) of
        true -> string:to_upper(C);
        _ -> case lists:member(C, lists:seq($A, $Z)) of
                true -> string:to_lower(C);
                false -> C
            end
    end.

minimum([H|T])->
    hd(lists:sort([H|T])).

minimum2([H|T]) ->
    min_helper(H, T).

min_helper(Min, []) ->
    Min;
min_helper(Min, [H|T]) ->
    if H < Min -> min_helper(H, T);
       true -> min_helper(Min, T)
    end.
% minimum([2,3,1,5]) == 1
zip([], _)->[];
zip(_, [])->[];
zip([H1|T1], [H2|T2])-> [{H1,H2}] ++ zip(T1,T2).
%zip ([1,2], "abc")= [{1, 'a'}, {2, 'b'}]

tails([])->[];
tails([H|T])->
    [[H|T]] ++ tails(T).

%-- `squares l` returns the list of the squares of the elements of xs.
%-- squares [2, 3, 5] == [4, 9, 25]
%squares :: [Int] -> [Int]
squares(Lst)->
    [trunc(math:pow(El,2)) || El <- Lst].

pow(B,P) when P > 1->
    B * pow(B,P-1);
pow(B,1) ->
    B.

squares2([H|T])->
    [pow(H,2) | squares(T)].

%-- `evens xs` keeps the even elements of xs.
%-- evens [5, 8, 10, 13, 16] == [8, 10, 16]
%evens :: [Int] -> [Int]

evens([])-> [];
evens([H|T]) ->
    case H rem 2 == 0 of
        true -> [H | evens(T)];
        false -> evens(T)
    end.

evens2(Lst)->
    [El || El <- Lst, El rem 2 == 0].

%-- `sums` computes all sums of an element of l1 with an element of l2.
%-- sums [10, 20] [1,3,5] == [11,13,15,21,23,25]
%sums :: [Int] -> [Int] -> [Int]
sums(L1,L2)->
    [El1 + El2 || El1 <-L1, El2<-L2].
sums2([], _) -> [];
sums2([H1|T1], L2) ->
    lists:map(fun(E) -> H1 + E end, L2) ++ sums(T1, L2).
%
%-- `countEven xs` should be the number of even elements in xs.
%-- countEvens [5, 8, 10, 13, 16] == 3
%countEven :: [Int] -> Int
countEvens(Lst)->
    length([El || El<-Lst, El rem 2 == 0]).

countEvens2(L)->countEvens2(L,0).
countEvens2([],Count)->Count;
countEvens2([H|T],Count)->
    case H rem 2 == 0 of
        true -> countEvens2(T,Count+1);
        false -> countEvens2(T,Count)
    end.

%-- `sumOfSquares n` should be the sum of the first n square numbers.
%sumOfSquares :: Int -> Int
sumOfSquares(N)->
    case N >= 0 of 
        true -> pow(N,2) + sumOfSquares(N-1);
        false -> 0
    end.

sumOfSquares2(N)->
    lists:sum([pow(S,2) || S <- lists:seq(0,N)]).    
%-- `isSquare n` should be True if n is a square number.
%isSquare :: Int -> Bool
isSquare(N) ->
    length([D || D<-lists:seq(2,N), N div D == D, N rem D == 0])==1.
%-- `divides` should check if `n` is a multiple of `d`
%divides :: Int -> Int -> Bool
%divides d n = n `mod` d == 0
divides(D,N)-> N rem D == 0.
%-- `divisors n` should be the lists of the divisors of n.
%-- divisors 28 == [1,2,4,7,14,28]
%divisors :: Int -> [Int]
divisors2(N)->
    [D || D <- lists:seq(1,N), divides(D,N)].
divisors3(N) -> divisors3(N,1).
divisors3(N,N)-> [N];
divisors3(N,D)->
    case divides(D,N) of
        true -> [D] ++ divisors3(N,D+1);
        false -> divisors3(N,D+1)
    end.


%
%-- `powersOf2 n` should consists of the first n powers of 2.
%-- powersOf2 6 == [1,2,4,8,16,32]
%powersOf2 :: Int -> [Int]
powersOf2(N) ->
    [1] ++ [pow(2, X) || X <- lists:seq(1, N-1)].

%
%-- `isPrime n` should check whether `n` is a prime number.
%isPrime :: Int -> Bool
%isPrime = undefined
isPrime(N) ->
    length([D || D <- lists:seq(1,N), N rem D == 0])==2.

isPrime2(N)-> isPrime2(N,1,0).
isPrime2(N,D,Count) when N >= D ->
    case N rem D == 0 of
        true -> isPrime2(N,D+1,Count+1);
        false -> isPrime2(N,D+1,Count)
    end;
isPrime2(_N,_D,Count)->Count==2.
%-- `primeBelow n` should be the list of all prime numbers in 2..n
%primesBelow :: Int -> [Int]
%primesBelow = undefined
primesBelow(N)->
    [I || I <- lists:seq(2,N), isPrime(I)].
%-- Check if all elements in a list are equal to each other!
%allEqual :: [Int] -> Bool
allEqual([]) -> true;
allEqual(L) ->
    length(freq(L)) == 1.

allEqual2([]) -> true;
allEqual2([_]) -> true;
allEqual2([X,X|Xs]) -> allEqual([X|Xs]);
allEqual2(_) -> false.
%-- Examples:
%-- ∙ allEqual []      == True
%-- ∙ allEqual [1,2]   == False
%-- ∙ allEqual [3,3,3] == True

%-- Zip is a function that pairs the elements of two lists.
%--  zip :: [a] -> [b] -> [(a,b)]
%-- Example: zip [0,1,2] ['a','b','c'] = [(0,'a'), (1,'b'), (2,'c')]
zip2(L1,L2)->
    [{El1,El2} || El1<-L1, El2<-L2].

zip3(_,[])->[];
zip3([],_)->[];
zip3([H1|T1],[H2|T2])->
    [{H1,H2} | zip3(T1,T2)].
%-- zip is often used to pair elements with their position
%--  zip [0..] "Hello" = [(0,'H'), (1, 'e'), (2, 'l'), (3, 'l'), (4, 'o')]
%
%-- Use zip to only keep the elements of a list that occur at an even position.
%elemsAtEvenPos :: [a] -> [a]
%elemsAtEvenPos = undefined
%-- Examples: 
%-- - elemsAtEvenPos "Hello" = "Hlo"
%-- - elemsAtEvenPos "abcdef" = "ace"
elemsAtEvenPos(Lst)->
    [El || {El,I} <- lists:zip(Lst,lists:seq(0,length(Lst)-1)), I rem 2 == 0].

elemsAtEvenPos2(Lst)-> elemsAtEvenPos2(Lst,0).
elemsAtEvenPos2([],_)-> [];
elemsAtEvenPos2([H|T],Idx)->
    case Idx rem 2 == 0 of
        true -> [H | elemsAtEvenPos2(T,Idx+1)];
        false ->elemsAtEvenPos2(T,Idx+1)
    end.

%-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at
%-- even and odd positions:
%-- (You can assume that the length of the input list is even.)
%-- Example:
%--  swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
%-- Hint: use zip
%swapEvenOddPos :: [Int] -> [Int]
swapEvenOddPos([])-> [];
swapEvenOddPos([H1,H2|T]) ->
    [H2,H1 | swapEvenOddPos(T)].

applyAllPar(FS,LS)->
    [F(E) || F<-FS, E <- LS].

applyAllPar2([],_)-> [];
applyAllPar2(_,[])-> [];
applyAllPar2([F|FS],LS)->
    [F(E) || E<-LS] ++ applyAllPar(FS,LS).
%full2:applyAllPar([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].
%full2:applyAllPar([fun(A) -> A+2 end], []) == [].
%full2:applyAllPar([], [apple, pear]) == [].
%full2:applyAllPar([fun erlang:is_list/1], [apple, pear]) == [false,false].
%full2:applyAllPar([fun erlang:is_list/1], [apple, pear, []]) == [false,false,true].
pmfm(_F, _G, _H, []) ->
    [];
pmfm(F, G, H, [X|XS]) ->
    try
        case G(F(X)) of
            true -> [H(F(X)) | pmfm(F, G, H, XS)];
            false -> pmfm(F, G, H, XS)
        end
    catch
        _:_ -> "wow"
    end.
    
%full2:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:atom_to_list/1, []) ==[]
%full2:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:atom_to_list/1, [1, apple, 2])==["apple"]   
%full2:pmfm(fun(X)-> X*2 end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [1, 2, 3, 4, 5, 6]) ==[1, 2, 3, 4, 5, 6]   
%full2:pmfm(fun(X)-> X end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [11, 12, 13, 14, 15, 16]) == [6,7,8]
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-2) + fib(N-1).

apply_alternately(F,G,L)-> apply_alternately(F,G,L,1).
apply_alternately(_F,_G,[],_Idx) -> [];
apply_alternately(F,G,[H|T],Idx) ->
    case Idx rem 2 == 0 of
        true -> [G(H) | apply_alternately(F,G,T,Idx+1)];
        false ->  [F(H) | apply_alternately(F,G,T,Idx+1)]
    end.


%full:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, [1,2,3,4,5]) == [2,4,4,8,6].
%full:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, [1,22,3,44,5]) == [2,44,4,88,6].
%full:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, []) == [].
%full:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, [1,22]) == [2,44].
%full:apply_alternately(fun full:fib/1, fun(E) -> E*2 end, [35, 2]) == [14930352,4].
%full:apply_alternately(fun(E) -> E*2 end, fun full:fib/1, [35, 35, 2]) == [70,14930352,4].
%full:apply_alternately(fun(E) -> E*2 end, fun full:fib/1, [35, 38, 2]) == [70,63245986,4].
%full:apply_alternately(fun full:fib/1, fun(E) -> E*2 end, [38, 2, 39]) == [63245986,4,102334155].
%full:apply_alternately(fun(E) -> E + 1 end, fun erlang:atom_to_list/1, [1,apple]) == [2,"apple"].
%full:apply_alternately(fun(E) -> E + 1 end, fun erlang:atom_to_list/1, [1, apple, 3, pear, 4, plum]) == [2,"apple",4, "pear",5, "plum"].
%full:apply_alternately(fun full:fib/1, fun erlang:atom_to_list/1, [1, apple, 3, pear, 4, plum]) == [1, "apple",3, "pear", 5, "plum"].
%full:apply_alternately(fun full:fib/1, fun erlang:atom_to_list/1, [1, apple, 33, pear, 40, plum]) == [1, "apple",5702887, "pear",165580141, "plum"].
%full:apply_alternately(fun full:fib/1, fun erlang:atom_to_list/1, [40, apple, 33, pear, 3, plum]) == [165580141, "apple",5702887, "pear",3, "plum"].
equals(L1,L2)->equals(L1,L2,1).
equals([],_,_)->[];
equals(_,[],_)->[];
equals([H1|T1],[H2|T2],Idx) when H1 == H2 ->
    [Idx | equals(T1,T2,Idx+1)];
equals([_H1|T1],[_H2|T2],Idx)->
    equals(T1,T2,Idx+1).

equals2(L1,L2)->equals(L1,L2,1).
equals2([],_,_)->[];
equals2(_,[],_)->[];
equals2([H1|T1],[H2|T2],Idx)->
    case H1 == H2 of
        true -> [Idx | equals(T1,T2,Idx+1)];
        false -> equals(T1,T2,Idx+1)
    end.

%[Idx || {E1,E2,Idx} <- lists:zip3(L1,L2,lists:seq(0,length(L2))), E1 == E2].

%full:equals([],[]) == []
%full:equals([1],[]) == []
%full:equals([1,1,1],[1,2,1])  == [1,3]
%full:equals([1,1,1,2,2,2,3,3,3],[1,2,1,1,2,1]) == [1,3,5]
%full:equals([1,1,1,2,2,2,3,3,3],[1,2,1,1,2,1,3,3,3,3,3,3,3]) == [1,3,5,7,8,9]
%full:equals([1,1,1,2,2,2,3,3,3],"abcdefg") == []
%full:equals([1,1,1,2,2,2,3,3,3],[a,b,c,d]) == []
%full:equals("firstlist", "secondlist") == []
%full:equals(" firstlist", "secondlist") == [7,8,9,10]

reduce_alter([]) ->
    not_defined;
reduce_alter([X]) ->
    X;
reduce_alter([A,B|T]) ->
    helper_reduce(T, A + B, 3).

helper_reduce([], Acc,_) ->
    Acc;
helper_reduce([H|T], Acc,Idx) when Idx rem 2 /= 0 ->
    helper_reduce(T, Acc * H,Idx+1);
helper_reduce([H|T], Acc,Idx)->
    helper_reduce(T, Acc + H,Idx+1).



%full:reduce_alter([212,313,414,515,616]) == 134204840
%full:reduce_alter([]) == not_defined
%full:reduce_alter([212]) == 212
%full:reduce_alter([1,1,1,1,1,1]) == 4
%full:reduce_alter([1,2,1,2,1,2,1]) == 7
%full:reduce_alter([111,222,333,444]) == 111333
%full:reduce_alter([10,10,10,10,10,10,10]) == 21100



%full:filter(fun erlang:is_integer/1, [3], [3]) == #{false => not_found,true => {3,3}}
%full:filter(fun erlang:is_integer/1, [0, 1,2,3, 6], [0, 1,2,3, 5]) == #{false => not_found,true => {6,5}}
%full:filter(fun erlang:is_integer/1, [0, 1,2,d, 3, 6], [0, 1,2,d, 3, 5])  == #{false => {d,d},true => {6,5}}
%full:filter(fun erlang:is_integer/1, [0, 1,2,d, 3, 6, 5], [0, 1,2,d, 3, 5, d])  == #{false => {d,d},true => {6,5}}
%full:filter(fun erlang:is_integer/1, [a], [a,d,f]) == #{false => {a,a},true => not_found}
%full:filter(fun erlang:is_integer/1, [a,d,f,g,g], [a,d,f]) == #{false => {f,f},true => not_found}
%full:filter(fun(X) -> X + 1 < 1 end, [a,d,f,g,g], [a,d,f])  == #{false => not_found,true => not_found}
%full:filter(fun(X) -> X + 1 < 11 end, [1,d,f,g,g], [1,d,f]) == #{false => not_found,true => {1,1}}
%full:filter(fun(X) -> X + 1 < 11 end, [1,11, d,f,g,g], [1,11,d,f]) == #{false => {11,11},true => {1,1}}
%full:filter(fun(X) -> X + 1 < 11 end, [1,11, d,f,g,g], [1,11, 12, d,f])  == #{false => {11,11},true => {1,1}}
%full:filter(fun(X) -> X + 1 < 11 end, [], []) == #{false => not_found,true => not_found}
%full:filter(fun(X) -> X + 1 < 11 end, [], [h])  == #{false => not_found,true => not_found}
%full:filter(fun(X) -> X + 1 < 11 end, [], [1]) == #{false => not_found,true => not_found}

zip4([],_)->[];
zip4(_,[])->[];
zip4([H1|T1],[H2|T2])-> [{H1,H2} | zip4(T1,T2)].
multi(F,L1,L2)->
    MainPid  = self(),
    Pids = [spawn(fun()-> MainPid ! multiApplyFun(F,E1,E2) end) || {E1,E2} <- zip4(L1,L2)],
    Res = [receive Val -> Val  end || _ <- Pids],
    errorHandler(Res).

errorHandler([])->[];
errorHandler([H|T])->
    case H of
        {Err,Reason} -> {Err,Reason};
        Val -> [Val | errorHandler(T)]
    end.

multiApplyFun(F,E1,E2)->
    try
        F(E1,E2)
    catch
        _:_ -> {'EXIT', "Non matching types"}
    end.

%full:multi(fun erlang:'+'/2, [], []) == []
%full:multi(fun erlang:'+'/2, [1,2], [3,4]) == [4,6]
%full:multi(fun erlang:'+'/2, [1,2,5], [3,4]) == [4,6]
%full:multi(fun erlang:'+'/2, [1,2,5], [3,4, 8, 9]) == [4,6,13]
%full:multi(fun(X, Y) -> {X, Y, math:pow(X, Y)} end, [1,2,5], [3,4, 8, 9]) == [{1,3,1.0},{2,4,16.0},{5,8,390625.0}]
%full:multi(fun erlang:'+'/2, [1], [apple]) == {'EXIT',"Non matching types"}

pany(F,L)->
    MainPid = self(),
    Pids = [spawn(fun()-> MainPid ! {F(E),E} end) || E <- L],
    Res = [receive Val -> Val end],
    checkIfTrue(Res).


checkIfTrue([])-> false;
checkIfTrue([H|T])->
    case H of
        {false,Bool} -> checkIfTrue(T);
        {true, Elem} -> {true, Elem}
    end.

%full:pany(fun(X) -> X > 6 end, [1,2,3]) == false
%full:pany(fun(X) -> X > 6 end, [11,12,13]) == {true, 11} or {true, 12} or {true, 13}
%full:pany(fun erlang:is_atom/1, [1,apple,2]) == {true, apple}

applyAllPar3(FS,LS)->
    MainPid = self(),
    Pids = [spawn(fun()-> MainPid ! {self(),F(E)} end) || F <- FS, E <- LS],
    [receive {Pid,Val} -> Val end || Pid <- Pids].
%full:applyAllPar3([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].
%full:applyAllPar3([fun(A) -> A+2 end], []) == [].
%full:applyAllPar3([], [apple, pear]) == [].
%full:applyAllPar3([fun erlang:is_list/1], [apple, pear]) == [false,false].
%full:applyAllPar3([fun erlang:is_list/1], [apple, pear, []]) == [false,false,true].

speculativeEval([],[]) -> no_proper_result;
speculativeEval([],_) -> no_proper_result;
speculativeEval(_,[]) -> no_proper_result;
speculativeEval(FS,LS) ->
    Main = self(),
    Pids = [spawn(fun() -> Main ! applySpeculative(F,El) end) || {F,El} <- zip(FS,LS)],
    Result = receive Value when is_number(Value) -> Value end,
    [receive A -> A end || _ <- lists:seq(1, length(Pids) - 1)] , Result.

applySpeculative(F,E)->
    try
        F(E)
    catch
        _:_ -> no_proper_result
    end.

%full:speculativeEval([], []) == no_proper_result.
%full:speculativeEval([fun full:fib/1, fun full:fib/1,fun full:fib/1, fun full:fib/1], []) == no_proper_result.
%full:speculativeEval([], [10,20,21,22]) == no_proper_result.
%full:speculativeEval([fun full:fib/1, fun full:fib/1,fun full:fib/1, fun full:fib/1], [10,20,21,22]) == 55. %% very probably
%full:speculativeEval([fun full:fib/1, fun full:fib/1,fun full:fib/1, fun full:fib/1], [20,10,21,22]) == 55. %% very probably
%full:speculativeEval([fun full:fib/1, fun full:fib/1,fun full:fib/1, fun full:fib/1], [22,24,20,10,21,22]) == 55.
%full:speculativeEval([fun full:fib/1, fun full:fib/1,fun full:fib/1, fun full:fib/1], [apple, pear, 22, plum, foo, bar]) == 17711.
%full:speculativeEval([fun full:fib/1, fun full:fib/1,fun full:fib/1, fun full:fib/1], [apple, pear, 22, plum, foo, bar]) == 17711.
%full:speculativeEval([fun full:fib/1, fun full:fib/1,fun full:fib/1, fun full:fib/1], [apple, pear, 22, plum, 23, bar]) == 17711.
%full:speculativeEval([fun full:fib/1, fun full:fib/1,fun full:fib/1, fun full:fib/1], [apple, pear, plum, orange, foo, bar]) == no_proper_result

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
    FinnPid = spawn(fun()-> finn(JakePid) end).

%full:begin_adventure().

pipe(SPid)->
    receive
        {forward, N} -> SPid ! N + 1, pipe(SPid);
        quit ->
            SPid ! quit
    end.

%start()->
%    E = spawn(fun() -> pipe(self()) end),
%    D = spawn(fun() -> pipe(E) end),
%    C = spawn(fun() -> pipe(D) end),
%    B = spawn(fun() -> pipe(C) end),
%    A = spawn(fun() -> pipe(B) end),
%    P = spawn(fun() -> pipe(A) end),
%    fun
%        (quit) ->
%            A ! quit,
%            receive
%                Msg -> Msg
%            end;
%        (N) ->
%            P ! {forward, N},
%            receive
%                Msg -> Msg
%            end
%    end.

player(NextPid) ->
    receive
        Msg when Msg rem 3 == 0 ->
            NextPid ! Msg;
        Msg ->
            NewMsg = Msg * 10 + (3 - (Msg rem 3)),
            NextPid ! NewMsg
    end.
start(InitMsg) ->
    Pid5 = spawn_link(full, player, [[]]),
    Pid4 = spawn_link(full, player, [Pid5]),
    Pid3 = spawn_link(full, player, [Pid4]),
    Pid2 = spawn_link(full, player, [Pid3]),
    Pid1 = spawn_link(full, player, [Pid2]),
    Pid1 ! InitMsg,
    receive
        FinalMsg -> FinalMsg
    end.


% full:start(1) == 12333

merge_sort([]) -> [];
merge_sort(L) when length(L) == 1 -> L;
merge_sort(L)->
    MainPid = self(),
    {L1,L2}=lists:split(length(L) div 2, L),
    spawn(fun()-> MainPid ! merge_sort(L1) end),
    spawn(fun()-> MainPid ! merge_sort(L2) end),
    receive
        Val1 -> Val1
    end,
    receive
        Val2-> Val2
    end,
    lists:merge(Val1,Val2).

%full:merge_sort([111,11,1,12,13,23,2,3,31,22,253,4,221]) == [1,2,3,4,11,12,13,22,23,31,111,221,253]
%full:merge_sort([5,4,3]) == [3,4,5]
%full:merge_sort([5,4,3,5,1,3,2]) == [1,2,3,3,4,5,5]
%full:merge_sort([5,4,3,1,2]) == [1,2,3,4,5]
%full:merge_sort([5,4,3,1,2, 0, -3, -211]) == [-211,-3,0,1,2,3,4,5]
apply_alternately(FS,L)->
    MainPid = self(),
    Res = [spawn(fun()-> MainPid ! {Idx,F(E)} end) || {F,E}<- zip(FS,L), Idx <- lists:seq(1,length(L)), Idx rem 2 /= 0] ++
    [spawn(fun()-> MainPid ! {Idx,F(E)} end) || {F,E}<- zip(FS,L), Idx <- lists:seq(1,length(L)), Idx rem 2 == 0],
    [receive
        {Idx,Val}-> Val
    end|| _ <- list:seq(1,Res)].

%full:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, [1,2,3,4,5]) == [2,4,4,8,6].
%full:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, [1,22,3,44,5]) == [2,44,4,88,6].
%full:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, []) == [].
%full:apply_alternately(fun(E) -> E + 1 end, fun(E) -> E*2 end, [1,22]) == [2,44].
%full:apply_alternately(fun ptest:fib/1, fun(E) -> E*2 end, [35, 2]) == [14930352,4].
%full:apply_alternately(fun(E) -> E*2 end, fun ptest:fib/1, [35, 35, 2]) == [70,14930352,4].
%full:apply_alternately(fun(E) -> E*2 end, fun ptest:fib/1, [35, 38, 2]) == [70,63245986,4].
%full:apply_alternately(fun ptest:fib/1, fun(E) -> E*2 end, [38, 2, 39]) == [63245986,4,102334155].
%full:apply_alternately(fun(E) -> E + 1 end, fun erlang:atom_to_list/1, [1,apple]) == [2,"apple"].
%full:apply_alternately(fun(E) -> E + 1 end, fun erlang:atom_to_list/1, [1, apple, 3, pear, 4, plum]) == [2,"apple",4, "pear",5, "plum"].
%full:apply_alternately(fun ptest:fib/1, fun erlang:atom_to_list/1, [1, apple, 3, pear, 4, plum]) == [1, "apple",3, "pear", 5, "plum"].
%full:apply_alternately(fun ptest:fib/1, fun erlang:atom_to_list/1, [1, apple, 33, pear, 40, plum]) == [1, "apple",5702887, "pear",165580141, "plum"].
%full:apply_alternately(fun ptest:fib/1, fun erlang:atom_to_list/1, [40, apple, 33, pear, 3, plum]) == [165580141, "apple",5702887, "pear",3, "plum"].


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