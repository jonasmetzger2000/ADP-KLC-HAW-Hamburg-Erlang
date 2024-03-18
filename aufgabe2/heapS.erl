%%%-------------------------------------------------------------------
%%% @author jonas
%%% @copyright (C) 2023, HAW Hamburg
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2023 17:23
%%%-------------------------------------------------------------------
-module(heapS).
-author("jonas").
%%-compile(export_all).
-export([heapS/1]).

% create new Heap
addToHeap(Num) -> {1, {Num, {}, {}}}.
% add to existing Heap
addToHeap({Size, Heap}, Num) -> {Size+1, addToHeap(Heap, Num, calcPath(Size+1))}.
addToHeap({}, Num, []) -> {Num, {}, {}};
addToHeap({N, L, R}, Num, [l|PathTail]) when Num > N -> { Num, addToHeap(L, N, PathTail), R};
addToHeap({N, L, R}, Num, [l|PathTail]) -> {N, addToHeap(L, Num, PathTail), R};
addToHeap({N, L, R}, Num, [r|PathTail]) when Num > N -> { Num, L, addToHeap(R, N, PathTail)};
addToHeap({N, L, R}, Num, [r|PathTail]) -> {N, L, addToHeap(R, Num, PathTail)}.

% Get Bottom
getBottom({Size, Heap}) -> getBottom(calcPath(Size), Heap).
getBottom([], {N, _, _}) -> N;
getBottom([Path|PathTail], {_N, L, _R}) when Path == l -> getBottom(PathTail, L);
getBottom([Path|PathTail], {_N, _L, R}) when Path == r -> getBottom(PathTail, R).

% Switches Top with Bottom, removing Bottom
switchBottomToTop({Size, {T, L, R} = Heap}) -> {T, {Size-1, switchBottomToTop({getBottom({Size, Heap}), L, R}, calcPath(Size))}}.
switchBottomToTop({_N, _L, _R}, []) -> {};
switchBottomToTop({N, L, R}, [Path|PathTail]) when Path == l -> {N, switchBottomToTop(L, PathTail), R};
switchBottomToTop({N, L, R}, [Path|PathTail]) when Path == r -> {N, L, switchBottomToTop(R, PathTail)}.

% heapify
heapify({Size, Heap}) -> {Size, heapify2(Heap)}.
heapify2({}) -> {};
heapify2({_Num, {}, {}} = H) -> H;
heapify2({N, {NL, _LL, _LR}, {}} = H) when (N >= NL) -> H;
heapify2({N, {NL, LL, LR}, {}}) when (N < NL) -> {NL, heapify2({N, LL, LR}), {}};
heapify2({N, {NL, _LL, _LR}, {NR, _RL, _RR}} = H) when (N >= NL) and (N >= NR) -> H;
heapify2({N, {NL, _LL, _LR} = Left, {NR, RL, RR}}) when ((N < NL) or (N < NR)) and (NL =< NR) -> {NR, Left, heapify2({N, RL, RR})};
heapify2({N, {NL, LL, LR}, {NR, _RL, _RR} = Right}) when ((N < NL) or (N < NR)) and (NL > NR) -> {NL, heapify2({N, LL, LR}), Right}.

heapFromList([Head|Tail]) -> heapFromList(Tail, addToHeap(Head)).
heapFromList([], Heap) -> Heap;
heapFromList([Head|Tail], Heap) -> heapFromList(Tail, addToHeap(Heap, Head)).

heapS(List) -> heapS(heapFromList(List), []).
heapS({0, _}, Res) -> Res;
heapS(Heap, Res) -> {Max, NewHeap} = switchBottomToTop(Heap), heapS(heapify(NewHeap), [Max] ++ Res).

calcPath(Number) -> calcPath(Number,[]).
calcPath(1,Accu) -> Accu;
calcPath(Number,Accu) when Number rem 2 =:= 0 -> calcPath(Number div 2,[l|Accu]);
calcPath(Number,Accu) when Number rem 2 =/= 0 -> calcPath((Number-1) div 2,[r|Accu]).