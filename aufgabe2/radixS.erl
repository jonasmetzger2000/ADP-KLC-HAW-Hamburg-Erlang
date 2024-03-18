-module(radixS).
%%-compile(export_all).
-export([radixS/2]).

radixS(List, MaxDigit) -> radixS(List, MaxDigit+1, 1).
radixS(List, MaxDigit, MaxDigit) -> List;
radixS(List, MaxDigit, N) -> radixS(
  (collect(partition(List, N))), MaxDigit, N+1).

partition(List, Stelligkeit) -> partition(List, Stelligkeit, bucket()).
partition([], _Stelligkeit, Bucket) -> Bucket;
partition([Head|Tail], Stelligkeit, Bucket) ->
  partition(Tail, Stelligkeit, appendToBucket(Bucket, assess(Head, Stelligkeit), Head)).

collect(Bucket) -> collect(Bucket, []).
collect([], Res) -> Res;
collect([{_N, List}|Tail], Res) -> collect(Tail, Res ++ lists:reverse(List)).

appendToBucket(List, Key, Element) -> appendToBucket(List, Key, Element, []).
appendToBucket([], _Key, _Element, Res) -> Res;
appendToBucket([{Key, List}|Tail], Key, Element, Res) -> Res ++ appendToBucket(Tail, Key, Element, [{Key, [Element] ++ List}]);
appendToBucket([{I, List}|Tail], Key, Element, Res) -> appendToBucket(Tail, Key, Element, Res ++ [{I, List}]).
bucket() -> [
  {0, []},
  {1, []},
  {2, []},
  {3, []},
  {4, []},
  {5, []},
  {6, []},
  {7, []},
  {8, []},
  {9, []}
].

% berechnet die Stelle der Zahl
assess(Num, Place) when Place > 0 ->
  Assess = util:float_to_int(math:pow(10,Place)),
  (Num - Assess * (Num div Assess)) div (Assess div 10).