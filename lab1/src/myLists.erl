-module(myLists).
-author("piotrek").

-export([contains/2, duplicateElements/1, sumFloats/1, sumFloatsTail/1]).


contains([], _) -> false;
contains([H | T], Value) ->
  case H of
    Value -> true;
    _ -> contains(T, Value)
  end.


duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).


sumFloats([]) -> 0.0;
sumFloats([H | T]) when is_float(H) -> H + sumFloats(T);
sumFloats([_ | T]) -> sumFloats(T).


sumFloatsTail([]) -> 0.0;
sumFloatsTail(List) -> sumFloatsTail_(List, 0).
sumFloatsTail_([], Sum) -> Sum;
sumFloatsTail_([H | T], Sum) when is_float(H) -> sumFloatsTail_(T, Sum + H);
sumFloatsTail_([_ | T], Sum) -> sumFloatsTail_(T, Sum).
